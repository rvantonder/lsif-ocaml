open Core
open Base64

module Time = Core_kernel.Time_ns.Span
module Json = Yojson.Safe

let debug = Option.is_some (Sys.getenv "DEBUG_OCAML_LSIF")

let i : int ref = ref 1

let fresh () =
  let id = !i in
  i := !i + 1;
  id

module Export = struct
  type tool_info =
    { name : string
    ; version : string
    }
  [@@deriving yojson]

  type content =
    { language : string
    ; value : string
    }
  [@@deriving yojson]

  type hover =
    { contents : content list
    }
  [@@deriving yojson]

  type location =
    { line : int
    ; character : int
    }
  [@@deriving yojson]

  type result =
    | Hover of hover

  let result_to_yojson = function
    | Hover contents -> hover_to_yojson contents

  let result_of_yojson _ = assert false

  type entry =
    { id : string
    ; entry_type : string [@key "type"]
    ; label : string
    ; result : result option
          [@default None]
    ; start : location option
          [@default None]
    ; end_ : location option
          [@key "end"]
          [@default None]
    ; version : string option
          [@default None]
    ; project_root : string option
          [@key "projectRoot"]
          [@default None]
    ; position_encoding : string option
          [@key "positionEncoding"]
          [@default None]
    ; tool_info : tool_info option
          [@key "toolInfo"]
          [@default None]
    ; kind : string option
          [@default None]
    ; uri : string option
          [@default None]
    ; language_id : string option
          [@key "languageId"]
          [@default None]
    ; contents : string option
          [@default None]
    ; out_v : string option
          [@key "outV"]
          [@default None]
    ; in_v : string option
          [@key "inV"]
          [@default None]
    ; out_vs : string list option
          [@key "outVs"]
          [@default None]
    ; in_vs : string list option
          [@key "inVs"]
          [@default None]
    }
  [@@deriving yojson]

  let default =
    { id = "-1"
    ; entry_type = ""
    ; label = ""
    ; result = None
    ; start = None
    ; end_ = None
    ; version = None
    ; project_root = None
    ; position_encoding = None
    ; tool_info = None
    ; kind = None
    ; uri = None
    ; language_id = None
    ; contents = None
    ; out_v = None
    ; in_v = None
    ; out_vs = None
    ; in_vs = None
    }
end

(** Merlin responses. *)
module Import = struct
  type t =
    { start_line : int
    ; start_character : int
    ; end_line : int
    ; end_character : int
    ; type_info : string
    }
end

let connect ?in_v ?out_v ~label =
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "edge"
  ; label
  ; out_v
  ; in_vs = Some [Option.value_exn in_v]
  }

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
  In_channel.close stdout;
  In_channel.close stderr;
  Out_channel.close stdin;
  let finished = Unix.waitpid pid in
  result

let lookup_dot_merlin filename =
  let dot_merlin_path = Filename.dirname filename ^/ ".merlin" in
  if Sys.is_file dot_merlin_path = `Yes then
    begin
      if debug then Format.printf "Merlin: %s@." dot_merlin_path;
      Some dot_merlin_path
    end
  else
    begin
      if debug then Format.printf "NO MERLIN: %s@." dot_merlin_path;
      None
    end

let call_merlin ~filename ~source ~line ~character ~query ~dot_merlin =
  let line = Int.to_string line in
  let character = Int.to_string character in
  let args =
    [ "server"
    ; query
    ; "-position"; line^":"^character
    ; "-index"; "0"
    ; filename
    ] @ dot_merlin
  in
  read_source_from_stdin args source

let to_lsif merlin_results : Export.entry list =
  let open Export in
  let open Option in
  let open Import in
  List.fold merlin_results ~init:[] ~f:(fun acc result ->
      if debug then Format.printf "Merlin result: %s@." result;
      let json = Json.from_string result in
      if debug then Format.printf "JSON: %s@." @@ Json.pretty_to_string json;
      let exported =
        let imported =
          match Json.Util.member "value" json with
          | `List [hd]
          | `List (hd::_) ->
            let start = Json.Util.member "start" hd in
            let start_line = Json.Util.member "line" start in
            let start_character = Json.Util.member "col" start in
            let end_ = Json.Util.member "end" hd in
            let end_line = Json.Util.member "line" end_ in
            let end_character = Json.Util.member "col" end_ in
            let type_info = Json.Util.member "type" hd in
            let read_int x = try Json.to_string x |> Int.of_string |> Option.some with _ -> None in
            let read_string x = try Json.to_string x |> Option.some with _ -> None in
            read_int start_line >>= fun start_line ->
            read_int start_character >>= fun start_character ->
            read_int end_line >>= fun end_line ->
            read_int end_character >>= fun end_character ->
            read_string type_info >>= fun type_info ->
            return { start_line; start_character; end_line; end_character; type_info }
          | `List [] -> None
          | _ -> failwith "Unexpected merlin result type in 'value' field."
        in
        imported >>= fun { start_line; start_character; end_line; end_character; type_info }  ->
        let result_set_id = fresh () in
        let result_set_vertex =
          { Export.default with
            id = Int.to_string result_set_id
          ; entry_type = "vertex"
          ; label = "resultSet"
          ; result = None
          }
        in
        let range_vertex =
          { Export.default with
            id = Int.to_string (fresh ())
          ; entry_type = "vertex"
          ; label = "range"
          ; start =
              Some
                { line = start_line - 1
                ; character = start_character
                }
          ; end_ =
              Some
                { line = end_line -1
                ; character = end_character
                }
          }
        in
        let json = Json.from_string result in
        (* connect range (outV) to resultSet (inV) *)
        let result_set_edge =
          { Export.default with
            id = Int.to_string (fresh ())
          ; entry_type = "edge"
          ; label = "next"
          ; out_v = Some range_vertex.id
          ; in_v = Some result_set_vertex.id
          }
        in
        let type_info_vertex =
          { Export.default with
            id = Int.to_string (fresh ())
          ; entry_type = "vertex"
          ; label = "hoverResult"
          ; result =
              Some
                (Hover
                   { contents = [
                         { language = "OCaml"
                         ; value = type_info
                         }
                       ]
                   })
          }
        in
        (* connect resultSet (outV) to hoverResult (inV) *)
        let hover_edge =
          { Export.default with
            id = Int.to_string (fresh ())
          ; entry_type = "edge"
          ; label = "textDocument/hover"
          ; out_v = Some result_set_vertex.id
          ; in_v = Some type_info_vertex.id
          }
        in
        return [hover_edge; type_info_vertex; result_set_edge; range_vertex; result_set_vertex]
      in
      match exported with
      | Some result -> result @ acc
      | None -> acc)
  |> List.rev

let process_file filename =
  if debug then Format.printf "File: %s@." filename;
  let query = "type-enclosing" in
  let dot_merlin =
    match lookup_dot_merlin filename with
    | Some dot_merlin -> ["-dot-merlin"; dot_merlin]
    | None -> []
  in
  let line_lengths =
    In_channel.read_lines filename
    |> List.map ~f:String.length
  in
  let lines = List.length line_lengths in
  let source = In_channel.read_all filename in
  let results =
    List.foldi line_lengths ~init:[] ~f:(fun line acc length ->
        Format.eprintf "%2.0f%%%!" ((Int.to_float line) /. (Int.to_float lines) *. 100.0);
        Format.eprintf "\x1b[999D";
        Format.eprintf "\x1b[2K";
        List.fold (List.range 0 length) ~init:acc ~f:(fun acc character ->
            (call_merlin ~filename ~source ~line:(line+1) ~character ~query ~dot_merlin)::acc))
    (* Remove merlin timing and notifications fields so we can dedup. *)
    |> List.map ~f:(fun s ->
        let json = Json.from_string s in
        let class_ = Json.Util.member "class" json in
        let value = Json.Util.member "value" json in
        Json.to_string @@ `Assoc ["class", class_; "value", value])
    |> List.dedup ~compare:String.compare
  in
  to_lsif results

let header () =
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "vertex"
  ; label = "metaData"
  ; version = Some "0.4.0"
  ; project_root = Some ("file://"^Sys.getcwd ())
  ; tool_info = Some { name = "lsif-ocaml"; version = "0.1.0" }
  ; position_encoding = Some "utf-16"
  }

let project () =
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "vertex"
  ; label = "project"
  ; kind = Some "OCaml"
  }

let document filepath =
  let contents_base64 =
    In_channel.read_all filepath
    |> Base64.Websafe.encode
  in
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "vertex"
  ; label = "document"
  ; uri = Some ("file://"^filepath)
  ; language_id = Some "OCaml"
  ; contents = Some contents_base64
  }

let connect_ranges results document_id =
  let open Export in
  let in_vs = List.filter_map results ~f:(function
      | { id; label = "range"; _ } -> Some id
      | _ -> None)
  in
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "edge"
  ; label = "contains"
  ; out_v = Some document_id
  ; in_vs = Some in_vs
  }

let () =
  match Sys.argv |> Array.to_list with
  | [] | [_] -> failwith "Supply a filename"
  | _ :: filepath :: _ ->
    let print =
      Fn.compose
        Json.to_string
        Export.entry_to_yojson
    in
    if debug then Format.printf "File: %s@." filepath;
    let header = header () in
    let project = project () in
    let document = document filepath in
    Format.printf "%s@." @@ print header;
    Format.printf "%s@." @@ print project;
    Format.printf "%s@." @@ print document;
    let document_project_edge =
      connect ~out_v:project.id ~in_v:document.id ~label:"contains"
    in
    Format.printf "%s@." @@ print document_project_edge;
    let results = process_file filepath in
    List.iter results ~f:(fun entry ->
        let entry = Export.entry_to_yojson entry in
        Format.printf "%s@." @@ Json.to_string entry);
    let edges = connect_ranges results document.id in
    let edges = Export.entry_to_yojson edges in
    Format.printf "%s@." @@ Json.to_string edges
