Pathname Module for OCaml
=========================

OCaml module to use pathnames as list or string.

### Available functions

See ```pathname.mli``` file.

### Example

      let path = Pathname.new_path_of_list ["foo"; "bar"] in
      let path' = Pathname.extend path "baz" in
      let path'' = Pathname.extend path' "moo/pli" in
      let big_path = Pathname.new_path_of_string "q/w/e/r/t/../y" in
      let file = Pathname.extend_file path'' "dog.jpg" in
      let parent = Pathname.parent path'' in

      print_endline (Pathname.to_string path'');
      print_endline (Pathname.to_string file);
      print_endline (Pathname.to_string big_path);
      print_endline (Pathname.extension file);
      print_endline (Pathname.no_extension file);
      print_endline (Pathname.to_string parent)

Compilation:

    ocamlc -I +str `ocamlfind query str`/str.cma pathname.ml example.ml

