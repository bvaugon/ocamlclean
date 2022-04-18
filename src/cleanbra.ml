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

open OByteLib.Normalised_instr

let clean code =
  let f i bc =
    match bc with
      | BRANCH ptr | BRANCHIF ptr | BRANCHIFNOT ptr | COMPBRANCH (_, _, ptr) ->
        if ptr = i + 1 then code.(i) <- Step1.nop;
      | _ -> ()
  in
  Array.iteri f code;
;;
