open! Core

module Position = struct
  type t =
    | Valid of
        { line_num : int
        ; pos_in_line : int
        }
    | Eof
  [@@deriving sexp_of]
end

type t =
  { source : File_path.t
  ; file_by_line : (char Array.t Array.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let read_one t ~from:pos =
  match (pos : Position.t) with
  | Eof -> None, pos
  | Valid { line_num; pos_in_line } ->
    let line = Array.get t.file_by_line line_num in
    let char = Array.get line pos_in_line in
    let (next_pos : Position.t) =
      match
        line_num = Array.length t.file_by_line - 1, pos_in_line = Array.length line - 1
      with
      (* Not end of line *)
      | _, false -> Valid { line_num; pos_in_line = pos_in_line + 1 }
      (* End of line, not on the last line *)
      | false, true -> Valid { line_num = line_num + 1; pos_in_line = 0 }
      (* End of line, on the last line *)
      | true, true -> Eof
    in
    Some char, next_pos
;;

let of_file_contents ?source s =
  let lines = String.split_on_chars s ~on:[ '\n' ] in
  let nonempty_lines = List.filter ~f:(fun xs -> not (String.is_empty xs)) lines in
  let file_by_line =
    List.map nonempty_lines ~f:(Fn.compose Array.of_list String.to_list) |> Array.of_list
  in
  let source = Option.value source ~default:(File_path.of_string "dummy") in
  { source; file_by_line }
;;

let start_of_file { file_by_line; _ } =
  if Array.length file_by_line = 0
  then Position.Eof
  else Valid { line_num = 0; pos_in_line = 0 }
;;
