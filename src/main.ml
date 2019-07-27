open Core

module Time = Core_kernel.Time_ns.Span

let debug = false

let i : int ref = ref 0

let fresh () =
  let id = !i in
  i := !i + 1;
  id

module Export = struct
  type content =
    { language : string
    ; value : string
    }
  [@@deriving yojson]

  type hover =
    { contents : content list }
  [@@deriving yojson]

  type location =
    { line : int
    ; character : int
    }
  [@@deriving yojson]

  type range =
    { start : location
    ; end_ : location [@key "end"]
    }
  [@@deriving yojson]

  type edge =
    { out_v : string [@key "outV"]
    ; in_v : string [@key "inV"]
    }
  [@@deriving yojson]

  type result =
    | Hover of hover
    | Range of range
    | Edge of edge

  let result_to_yojson = function
    | Edge edge -> edge_to_yojson edge
    | Hover contents -> hover_to_yojson contents
    | Range range -> range_to_yojson range

  let result_of_yojson _ = assert false

  type entry =
    { id : int
    ; entry_type : string [@key "type"]
    ; label : string
    ; result : result option [@default None]
    }
  [@@deriving yojson]
end

let read_with_timeout read_from_channels =
  let read_from_fds = List.map ~f:Unix.descr_of_in_channel read_from_channels in
  let read_from_channels =
    Unix.select
      ~restart:true
      ~read:read_from_fds
      ~write:[]
      ~except:[]
      ~timeout:(`After (Time.of_int_sec 1))
      ()
    |> (fun { Unix.Select_fds.read; _ } -> read)
    |> List.map ~f:Unix.in_channel_of_descr
  in
  List.map read_from_channels ~f:In_channel.input_all
  |> String.concat ~sep:"\n"

let read_source_from_stdin args source =
  let open Unix.Process_channels in
  let Unix.Process_info.{ stdin; stdout; stderr; pid } =
    Unix.create_process ~prog:"ocamlmerlin" ~args
  in
  let stdin = Unix.out_channel_of_descr stdin in
  let stdout = Unix.in_channel_of_descr stdout in
  let stderr = Unix.in_channel_of_descr stderr in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  let result = read_with_timeout [stdout; stderr] in
  let finished = Unix.waitpid pid in
  if debug then Format.printf "Fin: %s@." @@ Pid.to_string pid;
  result

let call_merlin ~filename ~line ~character =
  let query = "type-enclosing" in
  let line = Int.to_string line in
  let character = Int.to_string character in
  let args = ["server"; query; "-position"; line^":"^character; filename] in
  let source = In_channel.read_all filename in
  read_source_from_stdin args source

let to_lsif merlin_results : Export.entry list =
  let open Export in
  List.fold merlin_results ~init:[] ~f:(fun acc result ->
      let open Yojson.Safe.Util in
      if debug then Format.printf "Result: %s@." result;
      let json = Yojson.Safe.from_string result in
      if debug then Format.printf "json: %s@." @@ Yojson.Safe.pretty_to_string json;
      let start_line, start_character, end_line, end_character, type_ =
        json |> member "value" |>
        function
        | `List (hd::[])
        | `List (hd::_) ->
          let start = hd |> member "start" in
          let start_line = start |> member "line" in
          let start_character = start |> member "col" in
          let end_ = hd |> member "end" in
          let end_line = end_ |> member "line" in
          let end_character = end_ |> member "col" in
          let type_ = hd |> member "type" in
          start_line, start_character, end_line, end_character, type_
        | `List [] ->
          Format.printf "Empty list@.";
          `String "", `String "", `String "", `String "", `String ""
        | _ ->
          failwith "other"
      in
      let result_set_id = fresh () in
      let result_set_vertex =
        { id = result_set_id
        ; entry_type = "vertex"
        ; label = "resultSet"
        ; result = None
        }
      in
      let range_vertex =
        let open Option in
        let read x =
          try Yojson.Safe.to_string x |> Int.of_string |> Option.some with _ -> None
        in
        read start_line >>= fun start_line ->
        read start_character >>= fun start_character ->
        read end_line >>= fun end_line ->
        read end_character >>= fun end_character ->
        return
          { id = fresh ()
          ; entry_type = "vertex"
          ; label = "range"
          ; result =
              Some (Range
                      { start =
                          { line = start_line - 1
                          ; character = start_character
                          }
                      ; end_ =
                          { line = end_line - 1
                          ; character = end_character
                          }
                      })
          }
      in
      let json = Yojson.Safe.from_string result in
      let type_info = Yojson.Safe.to_string type_ in
      let type_info_vertex =
        { id = fresh ()
        ; entry_type = "vertex"
        ; label = "hoverResult"
        ; result =
            Some (Hover
                    { contents = [
                          { language = "OCaml"
                          ; value = type_info
                          }
                        ]
                    })
        }
      in
      (*
      let hover_edge =
        { id = fresh ()
        ; entry_type = "edge"
        ; label = "next"
        ; result =
            Some (Edge
                    { out_v = "out_v"
                    ; in_v = "in_"
                    })
        }
      in
      *)
      match range_vertex with
      | Some range_vertex ->
        (* connect range (outV) to resultSet (inV) *)
        let result_set_edge =
          { id = fresh ()
          ; entry_type = "edge"
          ; label = "next"
          ; result =
              Some
                (Edge
                   { out_v = Int.to_string range_vertex.id
                   ; in_v = Int.to_string result_set_vertex.id
                   })
          }
        in
        type_info_vertex::result_set_edge::range_vertex::result_set_vertex::acc
      | None ->
        acc)
  |> List.rev


let process_file filename =
  let line_lengths =
    In_channel.read_lines filename
    |> List.map ~f:String.length
  in
  let results =
    List.foldi line_lengths ~init:[] ~f:(fun line acc length ->
        List.fold (List.range 0 length) ~init:acc ~f:(fun acc character ->
            (call_merlin ~filename ~line:(line+1) ~character)::acc))
    (* remove timing and notifications fields *)
    |> List.map ~f:(fun s ->
        let open Yojson.Safe.Util in
        let json = Yojson.Safe.from_string s in
        let class_ = json |> member "class" in
        let value = json |> member "value" in
        Yojson.Safe.to_string @@ `Assoc ["class", class_; "value", value])
    |> List.dedup ~compare:String.compare
  in
  let results = to_lsif results in
  List.iter results ~f:(fun entry ->
      let entry = Export.entry_to_yojson entry in
      Format.printf "%s@." @@ Yojson.Safe.to_string entry)

let () =
  match Sys.argv |> Array.to_list with
  | [] | [_] -> failwith "Supply a filename"
  | _ :: filename :: _ ->
    Format.printf "File: %s@." filename;
    process_file filename
