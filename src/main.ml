open Core

module Time = Core_kernel.Time_ns.Span
module Json = Yojson.Safe

let debug = Option.is_some (Sys.getenv "DEBUG_OCAML_LSIF")
let emit_type_hovers = true
let emit_definitions = true

let i : int ref = ref 1

let fresh () =
  let id = !i in
  i := !i + 1;
  Int.to_string id

(* skip or continue directory descent *)
type 'a next =
  | Skip of 'a
  | Continue of 'a

let fold_directory root ~init ~f =
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
    ; document : int option
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
    ; document = None
    }

  module Vertex = struct
    let range start_line start_character end_line end_character =
      { default with
        entry_type = "vertex"
      ; label = "range"
      ; start =
          Some
            { line = start_line
            ; character = start_character
            }
      ; end_ =
          Some
            { line = end_line
            ; character = end_character
            }
      }

    let hover_result value =
      { default with
        entry_type = "vertex"
      ; label = "hoverResult"
      ; result =
          Some
            (Hover
               { contents = [
                     { language = "OCaml"
                     ; value
                     }
                   ]
               })
      }

    let definition_result () =
      { default with
        entry_type = "vertex"
      ; label = "definitionResult"
      ; result = None
      }

    let result_set () =
      { default with
        entry_type = "vertex"
      ; label = "resultSet"
      ; result = None
      }

  end
end

(** Merlin responses. *)
module Import = struct
  type location =
    { line : int
    ; col : int
    }
  [@@deriving of_yojson]

  type type_info =
    { start: location
    ; end_: location [@key "end"]
    ; type_ : string [@key "type"]
    }
  [@@deriving of_yojson]

  type definition_content =
    { file : string
    ; pos : location
    }
  [@@deriving of_yojson]

  type definition_info =
    { start: location
    ; end_: location [@key "end"]
    ; definition : definition_content
    }
  [@@deriving of_yojson]

  type t =
    | Type_info of type_info
    | Definition of definition_info
end

type hover_result_vertices =
  { result_set_vertex : Export.entry
  ; range_vertex : Export.entry
  ; type_info_vertex : Export.entry
  }

type definition_result_vertices =
  { declaration_result_set_vertex : Export.entry
  ; declaration_range_vertex : Export.entry
  ; reference_range_vertex : Export.entry
  ; definition_result_vertex : Export.entry
  ; destination_file : string
  }

type intermediate_result =
  { hovers : hover_result_vertices list
  ; definitions : definition_result_vertices list
  }

type filepath_hover_results =
  { filepath : string
  ; result : intermediate_result
  }

let connect ?out_v ?in_v ?in_vs ?document ~label () =
  if Option.is_some in_v then
    { Export.default with
      id = fresh ()
    ; entry_type = "edge"
    ; label
    ; out_v
    ; in_v
    ; document
    }
  else if Option.is_some in_vs then
    { Export.default with
      id = fresh ()
    ; entry_type = "edge"
    ; label
    ; out_v
    ; in_vs
    ; document
    }
  else
    failwith "Do not call with both in_v and in_vs"

let to_lsif merlin_results : intermediate_result =
  let open Export in
  let open Import in
  let open Option in
  let init = { hovers = []; definitions = [] } in
  List.fold merlin_results ~init ~f:(fun acc merlin_result ->
      if debug then Format.printf "Merlin result: %s@." merlin_result;
      let json = Json.from_string merlin_result in
      let result =
        match type_info_of_yojson json with
        | Ok t -> Some (Type_info t)
        | Error _ ->
          match definition_info_of_yojson json with
          | Ok d -> Some (Definition d)
          | Error _ ->
            if debug then Format.eprintf "Ignoring other json: %s@." @@ Json.pretty_to_string json;
            None
      in
      let exported =
        result >>= function
        | Type_info { start; end_; type_ } ->
          let result_set_vertex = Vertex.result_set () in
          let range_vertex = Vertex.range (start.line - 1) start.col (end_.line - 1) end_.col in
          let type_info_vertex = Vertex.hover_result type_ in
          return (`Hover { result_set_vertex; range_vertex; type_info_vertex })
        | Definition { start; end_; definition = { file; pos } } ->
          (* Create a resultSet vertex for the range where this definition is *)
          let declaration_result_set_vertex = Vertex.result_set () in
          (* Create the vertex range for it *)
          let declaration_range_vertex = Vertex.range (pos.line - 1) (pos.col - 1) (pos.line - 1) (pos.col - 1) in
          (* Create an edge with 'next' to connect resultSet and declaration range above. *)

          (* Create a range vertex for the reference range *)
          (*
          if end_.col = -1 then
            (* this happens when "not in environment type" *)
            failwith (Format.sprintf "end_.col is -1 for merlin result: %s" merlin_result);
*)
          let reference_range_vertex = Vertex.range (start.line - 1) start.col (end_.line - 1) end_.col in
          (* Create a 'next' edge to the result_set above *)

          (* Create a definitionResult vertex *)
          let definition_result_vertex = Vertex.definition_result () in
          (* Connect the definitionResult above to the resultset with textDocument/definition edge *)

          (* Add "item" edge to connect the definitionResult vertex to the range for a particular document (id) *)
          let destination_file = file in
          return
            (`Definition
               { declaration_result_set_vertex
               ; declaration_range_vertex
               ; reference_range_vertex
               ; definition_result_vertex
               ; destination_file
               })
      in
      match exported with
      | Some (`Hover result) -> { acc with hovers = result::acc.hovers }
      | Some (`Definition result) -> { acc with definitions = result::acc.definitions }
      | None -> acc)

let process_filepath filename =
  if debug then Format.printf "File: %s@." filename;
  In_channel.read_all (filename ^ ".lsif")
  |> String.split_lines
  |> to_lsif

let header host root =
  { Export.default with
    id = fresh ()
  ; entry_type = "vertex"
  ; label = "metaData"
  ; version = Some "0.4.0"
  ; project_root = Some ("file:///"^host^/root)
  ; tool_info = Some { name = "lsif-ocaml"; version = "0.1.0" }
  ; position_encoding = Some "utf-16"
  }

let project () =
  { Export.default with
    id = fresh ()
  ; entry_type = "vertex"
  ; label = "project"
  ; kind = Some "OCaml"
  }

let make_document host project_root relative_filepath absolute_filepath =
  let _contents_base64 =
    In_channel.read_all absolute_filepath
    |> Base64.Websafe.encode
  in
  { Export.default with
    entry_type = "vertex"
  ; label = "document"
  ; uri = Some ("file:///"^host^/project_root^/relative_filepath)
  ; language_id = Some "OCaml"
  (* FIXME *)
  ; contents = None
  }

let connect_ranges results document_id =
  let open Export in
  let in_vs = List.filter_map results ~f:(function
      | { id; label = "range"; _ } -> Some id
      | _ -> None)
  in
  connect ~out_v:document_id ~in_vs ~label:"contains" ()

let paths root =
  let f acc ~depth:_ ~absolute_path ~is_file =
    let is_ml_or_re_file =
      if is_file then
        [".ml"; ".mli" (*; ".re"; ".rei" *) ]
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
  fold_directory root ~init:[] ~f

let print =
  Fn.compose
    Json.to_string
    Export.entry_to_yojson

(* scheme:
   file:///<host>/<project_root>/prefx-stripped-from-local-absolute-root
*)
type flags =
  { host : string (* e.g., github.com. *)
  ; project_root : string (* under the host, e.g., github.com/project/root *)
  ; local_absolute_root : string (* absolute path of local root or file to index *)
  ; strip_prefix : string (* the prefix to strip from the absolute root *)
  (* ; type_info_only : bool *)
  (* ; include_base64 : bool *)
  }

let () =
  match Sys.argv |> Array.to_list with
  | _ :: local_absolute_root :: strip_prefix :: host :: project_root :: _ ->
    let paths = paths local_absolute_root in
    let header = header host project_root in
    let project = project () in
    Format.printf "%s@." @@ print header;
    Format.printf "%s@." @@ print project;
    let results = List.map paths ~f:(fun filepath -> { filepath; result = process_filepath filepath }) in
    (* Generate IDs and connect vertices sequentially. *)
    let document_id_table = String.Table.create () in
    List.iter results ~f:(fun { filepath = absolute_filepath; result = { hovers; definitions } } ->
        let relative_filepath = String.chop_prefix_exn absolute_filepath ~prefix:strip_prefix in
        let document = make_document host project_root relative_filepath absolute_filepath in
        (* add document to table. Not needed for type hovers... only definitions *)
        let document =
          match String.Table.find document_id_table absolute_filepath with
          | Some id -> { document with id = Int.to_string id }
          | None ->
            let id = Int.of_string (fresh ()) in
            String.Table.add_exn document_id_table ~key:absolute_filepath ~data:id;
            let document = { document with id = Int.to_string id } in
            Format.printf "%s@." @@ print document;
            let document_in_project_edge =
              connect ~out_v:project.id ~in_vs:[document.id] ~label:"contains" ()
            in
            Format.printf "%s@." @@ print document_in_project_edge;
            document
        in
        if emit_type_hovers then
          begin
            (* Reverse list for in-order printing *)
            let hovers = List.rev hovers in
            (* Emit type info *)
            let hovers =
              List.concat_map hovers ~f:(fun { result_set_vertex; range_vertex; type_info_vertex } ->
                  let result_set_vertex = { result_set_vertex with id = fresh () } in
                  let range_vertex = { range_vertex with id = fresh () } in
                  (* Connect range (outV) to resultSet (inV). *)
                  let result_set_edge =
                    connect ~out_v:range_vertex.id ~in_v:result_set_vertex.id ~label:"next" ()
                  in
                  let type_info_vertex = { type_info_vertex with id = fresh () } in
                  (* Connect resultSet (outV) to hoverResult (inV). *)
                  let hover_edge =
                    connect ~in_v:type_info_vertex.id ~out_v:result_set_vertex.id ~label:"textDocument/hover" ()
                  in
                  [result_set_vertex; range_vertex; result_set_edge; type_info_vertex; hover_edge]
                )
            in
            if hovers <> [] then
              begin
                List.iter hovers ~f:(fun entry -> Format.printf "%s@." @@ print entry);
                let edges_entry = connect_ranges hovers document.id in
                Format.printf "%s@." @@ print edges_entry;
              end
          end;
        (* Emit definitions *)
        if emit_definitions then
          begin
            let definitions =
              List.concat_map
                definitions
                ~f:(fun
                     { declaration_result_set_vertex
                     ; declaration_range_vertex
                     ; reference_range_vertex
                     ; definition_result_vertex
                     ; destination_file } ->
                     let declaration_result_set_vertex = { declaration_result_set_vertex with id = fresh () } in
                     let declaration_range_vertex = { declaration_range_vertex with id = fresh () } in
                     let reference_range_vertex = { reference_range_vertex with id = fresh () } in
                     let definition_result_vertex = { definition_result_vertex with id = fresh () } in
                     let declaration_result_set_to_declaration_range_edge =
                       connect
                         ~out_v:declaration_range_vertex.id
                         ~in_v:declaration_result_set_vertex.id
                         ~label:"next"
                         ()
                     in
                     let declaration_result_set_to_reference_range_edge =
                       connect
                         ~out_v:reference_range_vertex.id
                         ~in_v:declaration_result_set_vertex.id
                         ~label:"next"
                         ()
                     in
                     let definition_result_to_declaration_result_set_edge =
                       connect
                         ~out_v:declaration_result_set_vertex.id
                         ~in_v:definition_result_vertex.id
                         ~label:"textDocument/definition"
                         ()
                     in
                     let definition_result_to_document_and_range =
                       let destination_file =
                         match destination_file with
                         | "*buffer*" -> absolute_filepath
                         | filepath -> filepath
                       in
                       let document =
                         match String.Table.find document_id_table destination_file with
                         | Some id ->
                           (* Already printed the previous time this was added to the table *)
                           { document with id = Int.to_string id }
                         | None ->
                           let relative_filepath =
                             match String.chop_prefix destination_file ~prefix:strip_prefix with
                             | Some relative_filepath_in_project_path ->
                               relative_filepath_in_project_path
                             | None ->
                               destination_file (* somewhere else, probably .opam *)
                           in
                           let document = make_document host project_root relative_filepath absolute_filepath in
                           let id = Int.of_string (fresh ()) in
                           String.Table.add_exn document_id_table ~key:destination_file ~data:id;
                           let document = { document with id = Int.to_string id } in
                           Format.printf "%s@." @@ print document;
                           let document_in_project_edge =
                             connect ~out_v:project.id ~in_vs:[document.id] ~label:"contains" ()
                           in
                           Format.printf "%s@." @@ print document_in_project_edge;
                           document
                       in
                       connect
                         ~out_v:definition_result_vertex.id
                         ~in_vs:[declaration_range_vertex.id]
                         ~document:(Int.of_string document.id)
                         ~label:"item"
                         ()
                     in
                     let single_definition_result =
                       [ declaration_result_set_vertex
                       ; declaration_range_vertex
                       ; declaration_result_set_to_declaration_range_edge
                       ; reference_range_vertex
                       ; declaration_result_set_to_reference_range_edge
                       ; definition_result_vertex
                       ; definition_result_to_declaration_result_set_edge
                       ; definition_result_to_document_and_range
                       ]
                     in
                     (* can do this outside, but for lsif data it's easier to read when associated
                        with the document *)
                     List.iter single_definition_result ~f:(fun entry -> Format.printf "%s@." @@ print entry);
                     single_definition_result)
            in
            if definitions <> [] then
              let edges_entry = connect_ranges definitions document.id in
              Format.printf "%s@." @@ print edges_entry;
          end;
      );
  | _ ->
    Format.eprintf "local_root(/Users/merlin/src) strip_prefix(/Users/.../) host(github.com) project(ocaml/merlin)"
