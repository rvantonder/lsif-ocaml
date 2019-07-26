open Core

module Time = Core_kernel.Time_ns.Span

let debug = false

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

  type result =
    | Hover of hover
    | Range of { start : location; end_ : location }
    | Edge of { out_v : string; in_v : string }
    | Nothing

  let result_to_yojson = function
    | Hover contents -> hover_to_yojson contents
    | Range _
    | Edge _
    | Nothing -> `Null

  let result_of_yojson _ = assert false

  type entry =
    { id : int
    ; entry_type : string [@key "type"]
    ; label : string
    ; result : result
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
      let result_set_vertex =
        { id = -1
        ; entry_type = "vertex"
        ; label = "resultSet"
        ; result = Nothing
        }
      in
      let hover_edge =
        { id = -1
        ; entry_type = "edge"
        ; label = "next"
        ; result =
            Edge
              { out_v = "out_v"
              ; in_v = "in_"
              }
        }
      in
      let range_vertex =
        { id = -1
        ; entry_type = "vertex"
        ; label = "range"
        ; result =
            let start_line =
              try
                Yojson.Safe.to_string start_line |> Int.of_string
              with _ -> -1
            in
            let start_character =
              try
                Yojson.Safe.to_string start_character |> Int.of_string
              with _ -> -1
            in
            let end_line =
              try
                Yojson.Safe.to_string end_line |> Int.of_string
              with _ -> -1
            in
            let end_character =
              try
                Yojson.Safe.to_string end_character |> Int.of_string
              with _ -> -1
            in
            Range
              { start =
                  { line = start_line
                  ; character = start_character
                  }
              ; end_ =
                  { line = end_line
                  ; character = end_character
                  }
              }
        }
      in
      let json = Yojson.Safe.from_string result in
      let type_info = Yojson.Safe.to_string type_ in
      let type_info_vertex =
        { id = -1
        ; entry_type = "vertex"
        ; label = "hoverResult"
        ; result =
            Hover
              { contents = [
                    { language = "OCaml"
                    ; value = type_info
                    }
                  ] }
        }
      in
      range_vertex::type_info_vertex::acc)


let process_file filename =
  let line_lengths =
    In_channel.read_lines filename
    |> List.map ~f:String.length
  in
  let result =
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
  let result =
    to_lsif result
    |> List.map ~f:Export.entry_to_yojson
    |> List.map ~f:Yojson.Safe.to_string
  in
  Format.printf "Result: %s@." @@ String.concat ~sep:"\n" result

let () =
  match Sys.argv |> Array.to_list with
  | [] | [_] -> failwith "Supply a filename"
  | _ :: filename :: _ ->
    Format.printf "File: %s@." filename;
    process_file filename
