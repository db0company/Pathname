(* ************************************************************************** *)
(* Project: Pathname                                                          *)
(* Description: Ocsigen module to manipulate filesystem paths                 *)
(* Author: db0 (db0company@gmail.com, http://db0.fr/)                         *)
(* Latest Version is on GitHub: https://github.com/db0company/Pathname        *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(* Types                                                                      *)
(* ************************************************************************** *)

{shared{
(* * List of each dir names in reverse order                                  *)
(* * The string representation of the path                                    *)
type t = (string list * string)
}}

(* ************************************************************************** *)
(* Values                                                                     *)
(* ************************************************************************** *)

{server{
(* sep : string                                                               *)
(* The directory separator (Example: "/" for Unix)                            *)
let sep = Filename.dir_sep
}}

{client{
(* sep : string                                                               *)
(* The directory separator (Example: "/" for Unix)                            *)
let sep = "/"
}}

{shared{
(* empty : t                                                                  *)
(* An empty path                                                              *)
let empty = ([], "")
}}

(* ************************************************************************** *)
(* Split for Eliom (function not available on client side)                    *)
(* ************************************************************************** *)

(* split : string -> string -> string list                                    *)
(* Take a string, a string corresponding to a separator (must be a single     *)
(* on client side) and return a list of string with each words                *)
(* Example: (split "apple;banana;cherry" ";") -> ["apple";"banana";"cherry"]  *)

{client{
let split str sep_str =

  let sep = sep_str.[0] in

  let string_of_char_list l =
    let str = String.make (List.length l) '\000' in
    let rec aux idx = function
      | []   -> str
      | c::t -> (String.set str idx c; aux (idx+1) t)
    in aux 0 l in

  let next_word str sep base_idx =
    let rec aux idx acc =
      try
	let c = String.get str idx in
	if (c <> sep)
	then aux (idx + 1) (c::acc)
	else (string_of_char_list (List.rev (acc)), (idx+1))
      with Invalid_argument _ -> (string_of_char_list (List.rev acc), base_idx)
    in aux base_idx [] in

  let rec aux idx acc =
    let (w, next_index) = next_word str sep idx in
    if (idx <> next_index)
    then aux next_index (w::acc)
    else w::acc
  in List.filter (fun str -> str <> "") (List.rev (aux 0 []))

}}

{server{
  let split str sep =
    Str.split (Str.regexp sep) str
}}

(* ************************************************************************** *)
(* Constructors                                                               *)
(* ************************************************************************** *)

{shared{
(* new_path : unit -> t                                                       *)
(* Return a new empty path                                                    *)
let new_path () = empty

(* new_path_of_string : string -> t                                           *)
(* Return a new path initialized using a string                               *)
let new_path_of_string spath =
  let dirlist = split spath sep in
  let dirstr = String.concat sep dirlist in
  ((List.rev dirlist), dirstr)

(* new_path_of_list : string list -> t                                        *)
(* Return a new path initialized using a list                                 *)
let new_path_of_list lpath =
  (List.rev lpath, String.concat sep lpath)

(* ************************************************************************** *)
(* Operators                                                                  *)
(* ************************************************************************** *)

(* concat : t -> t -> t                                                       *)
(* Concatenate two paths and return the result                                *)
let concat (l1, s1) (l2, s2) =
  ((l2 @ l1), (s1 ^ sep ^ s2))

(* extend : t -> string -> t                                                  *)
(* Extend path dir, appends the directory to the path                         *)
let extend path extdir =
  concat path (new_path_of_string extdir)
  
(* extend_file : t -> string -> t                                             *)
(* Extend path with a filename. Works only with raw filename, not paths.      *)
(* More efficient than extend.                                                *)
let extend_file (l, s) filename =
  ((filename::l), (s ^ sep ^ filename))

(* ************************************************************************** *)
(* Get                                                                        *)
(* ************************************************************************** *)

(* to_string : t -> string                                                    *)
(* Return a string corresponding to the path                                  *)
let to_string (l, s) = s

(* to_list : t -> string list                                                 *)
(* Return a list of strings corresponding to the path                         *)
let to_list (l, s) = List.rev l

(* ************************************************************************** *)
(* Tools                                                                      *)
(* ************************************************************************** *)

(* filename : t -> string                                                     *)
(* Return the filename without the rest of the path                           *)
let filename (l, _) = List.hd l

(* parent : t -> t                                                            *)
(* Return the path without the last element                                   *)
(* Example: "foo/bar/baz" -> "foo/bar"                                        *)
let parent (l, _) =
  let new_list = match l with
    | h::t	-> t
    | []	-> [] in
  (new_list, String.concat sep (List.rev new_list))

(* extension : t -> string                                                    *)
(* Return the extansion of the given filename                                 *)
(* Example : "document.pdf" -> "pdf"                                          *)
let extension path =
  let f = filename path in
  let start = try (String.rindex f '.') + 1 with Not_found -> 0
  in try String.sub f start ((String.length f) - start)
    with Invalid_argument s -> ""

(* no_extension : t -> string                                                 *)
(* Return filename without its extension                                      *)
(* Example : "/foo/bar/document.pdf" -> "document"                            *)
let no_extension path =
  let f = filename path in
  let size =
    try (String.rindex f '.') with Not_found -> -1
  in try String.sub f 0 size with Invalid_argument s -> f

(* is_empty : t -> bool                                                       *)
(* Check if the path is empty                                                 *)
let is_empty (l, _) = match l with
  | [] -> true
  | _  -> false
}}
