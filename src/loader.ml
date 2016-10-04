(*************************************************************************)
(*                                                                       *)
(*                              OCamlClean                               *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

let load filename =
  let ic = open_in_bin filename in
  let file_length = in_channel_length ic in
  let index = Index.parse ic in
  let code = Code.parse ic index in
  let prim = Prim.parse ic index in
  let data = Data.parse ic index in
  let begn =  Dump.parse_beginning ic index in
  let code_length = snd (Index.find_section index Index.Code) in
  let data_length = snd (Index.find_section index Index.Data) in
  let prim_length = snd (Index.find_section index Index.Prim) in
  let dumps =
    List.map (Dump.parse ic index)
      [ Index.Dlpt ; Index.Dlls ; Index.Symb ; Index.Crcs (*; Index.Dbug*) ]
  in
  close_in ic;
  (code, prim, data, begn, dumps,
   code_length, data_length, prim_length, file_length)
;;
