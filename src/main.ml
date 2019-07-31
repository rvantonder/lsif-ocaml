open Core
open Base64
open Hack_parallel

module Time = Core_kernel.Time_ns.Span
module Json = Yojson.Safe

let debug = Option.is_some (Sys.getenv "DEBUG_OCAML_LSIF")
let tokenize = true
let parallel = true

let i : int ref = ref 1

let fresh () =
  let id = !i in
  i := !i + 1;
  id

(* skip or continue directory descent *)
type 'a next =
  | Skip of 'a
  | Continue of 'a

let fold_directory ?(sorted=false) root ~init ~f =
  let rec aux acc absolute_path depth =
    if Sys.is_file absolute_path = `Yes then
      match f acc ~depth ~absolute_path ~is_file:true with
      | Continue acc
      | Skip acc -> acc
    else if Sys.is_directory absolute_path = `Yes then
      match f acc ~depth ~absolute_path ~is_file:false with
      | Skip acc -> acc
      | Continue acc ->
        Sys.ls_dir absolute_path
        |> List.fold ~init:acc ~f:(fun acc subdir ->
            aux acc (Filename.concat absolute_path subdir) (depth + 1))
    else
      acc
  in
  aux init root (-1)

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

(** Intermediate type so that edges and IDs can be connected after parallel vertex generation. *)
type hover_result_vertices =
  { result_set_vertex : Export.entry
  ; range_vertex : Export.entry
  ; type_info_vertex : Export.entry
  }

type filepath_hover_results =
  { filepath : string
  ; hovers : hover_result_vertices list
  }

let connect ?out_v ?in_v ?in_vs ~label () =
  if Option.is_some in_v then
    { Export.default with
      id = Int.to_string (fresh ())
    ; entry_type = "edge"
    ; label
    ; out_v
    ; in_v
    }
  else if Option.is_some in_vs then
    { Export.default with
      id = Int.to_string (fresh ())
    ; entry_type = "edge"
    ; label
    ; out_v
    ; in_vs
    }
  else
    failwith "Do not call with both in_v and in_vs"

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
    Unix.create_process ~prog:"/Users/rvt/merlin/ocamlmerlin" ~args
  in
  let stdin = Unix.out_channel_of_descr stdin in
  let stdout = Unix.in_channel_of_descr stdout in
  let stderr = Unix.in_channel_of_descr stderr in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  let result = read_with_timeout [stdout] in
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

let call_merlin ~filename ~source ~query ~dot_merlin =
  let args =
    [ "server"
    ; query
    ; "-position"; "1:1" (* place holder hack *)
    ; "-index"; "0"
    ; filename
    ] @ dot_merlin
  in
  read_source_from_stdin args source

let to_lsif_hover merlin_results : hover_result_vertices list =
  let open Export in
  let open Option in
  let open Import in
  List.fold merlin_results ~init:[] ~f:(fun acc result ->
      if debug then Format.printf "Merlin result: %s@." result;
      let xjson =
        if result = "" then
          (* if its empty, just make it list [] which will make it None below *)
          `List []
        else
          Json.from_string result
      in
      (* the new merlin format returns the object as a single element in a list. unbox it. *)
      let hd = match xjson with
        | `List [`Assoc body] -> `Assoc body
        | `List (`Assoc body::_) -> `Assoc body
        | `List [] -> `Assoc []
        | json ->
          if debug then Format.eprintf "Ignoring other json: %s@." @@ Json.pretty_to_string json;
          `Assoc []
      in
      if debug then Format.printf "JSON: %s@." @@ Json.pretty_to_string hd;
      let exported =
        if hd = `Assoc [] then
          None
        else
          let imported =
            let start = Json.Util.member "start" hd in
            let start_line = Json.Util.member "line" start in
            let start_character = Json.Util.member "col" start in
            let end_ = Json.Util.member "end" hd in
            let end_line = Json.Util.member "line" end_ in
            let end_character = Json.Util.member "col" end_ in
            let type_info = Json.Util.member "type" hd in
            let read_int x = try Json.Util.to_int x |> Option.some with _ -> None in
            let read_string x = try Json.Util.to_string x |> Option.some with _ -> None in
            read_int start_line >>= fun start_line ->
            read_int start_character >>= fun start_character ->
            read_int end_line >>= fun end_line ->
            read_int end_character >>= fun end_character ->
            read_string type_info >>= fun type_info ->
            return { start_line; start_character; end_line; end_character; type_info }
          in
          imported >>= fun { start_line; start_character; end_line; end_character; type_info }  ->
          let result_set_vertex =
            { Export.default with
              entry_type = "vertex"
            ; label = "resultSet"
            ; result = None
            }
          in
          let range_vertex =
            { Export.default with
              entry_type = "vertex"
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
          let type_info_vertex =
            { Export.default with
              entry_type = "vertex"
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
          return { result_set_vertex; range_vertex; type_info_vertex }
      in
      match exported with
      | Some result -> result::acc
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
  let line_ranges =
    if not tokenize then
      In_channel.read_lines filename
      |> List.map ~f:String.length
      |> List.map ~f:(List.range 0)
    else
      (* Roughly, split each line into tokens, and put the cursor right after whitespace and tokens. *)
      let to_token_range line =
        let chars =
          ["!"; "\""; "#"; "$"; "%"; "&"; "'"; "*"; "+"
          ; ","; "-"; "."; "/"; ":"; ";"; "<"; "="; ">"
          ; "?"; "@"; ","; "("; ")"; "{"; "}"; "["; "]"
          ; "~"; "`"] in
        let type_at_locations =
          List.concat_map chars ~f:(fun pattern -> String.substr_index_all line ~may_overlap:false ~pattern)
        in
        let type_after_locations =
          type_at_locations
          |> List.map ~f:((+) 1)
        in
        String.substr_index_all line ~may_overlap:false ~pattern:" "
        |> List.map ~f:((+) 1)
        (* Always process start of line *)
        |> fun l -> 0::l@type_after_locations
      in
      In_channel.read_lines filename
      |> List.map ~f:to_token_range
  in
  let source = In_channel.read_all filename in
  let merlin_result = call_merlin ~filename ~source ~query ~dot_merlin in
  let merlin_result = String.split_lines merlin_result in
  to_lsif_hover merlin_result

let header host root =
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "vertex"
  ; label = "metaData"
  ; version = Some "0.4.0"
  ; project_root = Some ("file://"^host^root)
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

let document local_root local_subdir host project_root filepath =
  let contents_base64 =
    In_channel.read_all filepath
    |> Base64.Websafe.encode
  in
  let filepath_relative_project_root =
    let relative_filepath = String.chop_prefix_exn filepath ~prefix:local_root in
    project_root ^/ relative_filepath
  in
  { Export.default with
    entry_type = "vertex"
  ; label = "document"
  ; uri = Some ("file://"^host^filepath_relative_project_root)
  ; language_id = Some "OCaml"
  ; contents = Some contents_base64
  }

let connect_ranges results document_id =
  let open Export in
  let in_vs = List.filter_map results ~f:(function
      | { id; label = "range"; _ } -> Some id
      | _ -> None)
  in
  connect ~out_v:document_id ~in_vs ~label:"contains" ()

let paths root subdir =
  let f acc ~depth ~absolute_path ~is_file =
    let is_ml_or_re_file =
      if is_file then
        [".ml"; ".mli"; ".re"; ".rei"]
        |> List.exists ~f:(fun suffix -> String.is_suffix ~suffix absolute_path)
      else
        false
    in
    if is_ml_or_re_file then
      Continue (absolute_path::acc)
    else if Filename.basename absolute_path = "_build" then
      (* Don't descend into the _build directory. *)
      Skip acc
    else
      Continue acc
  in
  fold_directory (root^/subdir) ~init:[] ~f

let print =
  Fn.compose
    Json.to_string
    Export.entry_to_yojson

let process_filepath project_id filepath =
  if debug then Format.printf "File: %s@." filepath;
  process_file filepath

let () =
  Scheduler.Daemon.check_entry_point ();
  match Sys.argv |> Array.to_list with
  | _ :: local_root :: local_subdir :: host :: project_root :: n :: _ ->
    let number_of_workers = if parallel then Int.of_string n else 1 in
    let scheduler = Scheduler.create ~number_of_workers () in
    let paths = paths local_root local_subdir in
    let header = header host project_root in
    let project = project () in
    Format.printf "%s@." @@ print header;
    Format.printf "%s@." @@ print project;
    (* Get type information in parallel. *)
    let results =
      List.map paths ~f:(fun filepath -> { filepath; hovers = process_filepath project.id filepath })
    in
(*
    let results =
      Scheduler.map_reduce
        scheduler
        paths
        ~init:[]
        ~map:(fun all_document_results document_paths ->
            let documents_result =
              List.map document_paths ~f:(fun document_path ->
                  { filepath = document_path
                  ; hovers = process_filepath project.id document_path
                  })
            in
            documents_result@all_document_results)
        ~reduce:(@)
    in
   *)
    (* Generate IDs and connect vertices sequentially. *)
    List.iter results ~f:(fun { filepath; hovers } ->
        let document = document local_root local_subdir host project_root filepath in
        let document = { document with id = Int.to_string (fresh ()) } in
        Format.printf "%s@." @@ print document;
        let document_in_project_edge =
          connect ~out_v:project.id ~in_vs:[document.id] ~label:"contains" ()
        in
        Format.printf "%s@." @@ print document_in_project_edge;
        let hovers =
          List.concat_map hovers ~f:(fun { result_set_vertex; range_vertex; type_info_vertex } ->
              let result_set_vertex = { result_set_vertex with id = Int.to_string (fresh ()) } in
              let range_vertex = { range_vertex with id = Int.to_string (fresh ()) } in
              (* Connect range (outV) to resultSet (inV). *)
              let result_set_edge =
                connect ~out_v:range_vertex.id ~in_v:result_set_vertex.id ~label:"next" ()
              in
              let type_info_vertex = { type_info_vertex with id = Int.to_string (fresh ()) } in
              (* Connect resultSet (outV) to hoverResult (inV). *)
              let hover_edge =
                connect ~in_v:type_info_vertex.id ~out_v:result_set_vertex.id ~label:"textDocument/hover" ()
              in
              [result_set_vertex; range_vertex; result_set_edge; type_info_vertex; hover_edge]
            )
        in
        List.iter hovers ~f:(fun entry -> Format.printf "%s@." @@ print entry);
        let edges_entry = connect_ranges hovers document.id in
        Format.printf "%s@." @@ print edges_entry);
    begin
      try Scheduler.destroy scheduler
      with Unix.Unix_error (_,"kill",_) -> ()
    end
  | _ -> failwith {|Supply a "local root (absolute path)" "local subdir (will be x/y in /github.com/my/proj/x/y)" "/github.com" "project root (/my/proj)" "nprocs"|}
