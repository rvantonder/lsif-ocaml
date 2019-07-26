open Core

module Time = Core_kernel.Time_ns.Span

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
    | Nothing
  [@@deriving yojson]

  type entry =
    { id : int
    ; entry_type : string
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
  let _debug = Format.printf "Fin: %s@." @@ Pid.to_string pid in
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
      let result_set_vertex =
        { id = -1
        ; entry_type = "vertex"
        ; label = "resultSet"
        ; result = Nothing
        }
      in
      let range_vertex =
        { id = -1
        ; entry_type = "vertex"
        ; label = "range"
        ; result = Range
              { start =
                  { line = -1
                  ; character = -1
                  }
              ; end_ =
                  { line = -1
                  ; character = -1
                  }
              }
        }
      in
      let json = Yojson.Safe.from_string result in
      let type_info = "" in
      let range = "..." in
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
      }::acc)


let process_file filename =
  let line_lengths =
    In_channel.read_lines filename
    |> List.map ~f:String.length
  in
  let result =
    List.foldi line_lengths ~init:[] ~f:(fun line acc length ->
        List.fold (List.range 0 length) ~init:acc ~f:(fun acc character ->
            (call_merlin ~filename ~line:(line+1) ~character)::acc))
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
