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

let compute_used code =
  let nb_instr = Array.length code in
  let used = Array.make nb_instr false in
  let rec f i =
    if i < nb_instr && not used.(i) then (
      used.(i) <- true;
      match code.(i) with
        | Branch ptr ->
          f ptr.instr_ind;
        | Branchif ptr | Branchifnot ptr | Beq (_, ptr) | Bneq (_, ptr)
        | Blint (_, ptr) | Bleint (_, ptr) | Bgtint (_, ptr) | Bgeint (_, ptr)
        | Bultint (_, ptr) | Bugeint (_, ptr)
        | Pushretaddr ptr | Closure (_, ptr) | Pushtrap ptr ->
          f (succ i);
          f ptr.instr_ind;
        | Closurerec (_, _, o, t) ->
          f (succ i);
          f o.instr_ind;
          Array.iter (fun ptr -> f ptr.instr_ind) t;
        | Switch (_, tab) ->
          Array.iter (fun ptr -> f ptr.instr_ind) tab;
        | Grab _ -> f (pred i) ; f (succ i)
        | Return _ | Appterm (_, _) | Stop | Raise | Reraise | Raisenotrace -> ()
        | _ ->
          f (succ i)
    )
  in
  f 0;
  used
;;

let clean_code code used =
  let nb_instr = Array.length code in
  for i = 0 to nb_instr - 1 do
    if not used.(i) then code.(i) <- Nop;
  done
;;

let clean code =
  let used = compute_used code in
  clean_code code used;
;;
