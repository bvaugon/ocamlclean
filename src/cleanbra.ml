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

open Instr

let clean code =
  let f i bc =
    match bc with
      | Branch ptr | Branchif ptr | Branchifnot ptr | Beq (_, ptr)
      | Bneq (_, ptr) | Blint (_, ptr) | Bleint (_, ptr) | Bgtint (_, ptr)
      | Bgeint (_, ptr) | Bultint (_, ptr) | Bugeint (_, ptr) ->
        if ptr.instr_ind = i + 1 then
          code.(i) <- Nop
      | _ -> ()
  in
  Array.iteri f code;
;;
