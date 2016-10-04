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

let compute_map orig_code =
  let nb_instr = Array.length orig_code in
  let map = Array.make nb_instr 0 in
  let rec f i j =
    if i < nb_instr then (
      map.(i) <- j;
      match orig_code.(i) with
        | Nop | Pop 0 -> f (succ i) j
        | _ -> f (succ i) (succ j)
    )
  in
  f 0 0;
  map
;;

let compress orig_code map =
  let orig_size = Array.length orig_code in
  let new_size = map.(Array.length map - 1) + 1 in
  let new_code = Array.make new_size orig_code.(0) in
  for i = 0 to orig_size - 1 do
    new_code.(map.(i)) <- orig_code.(i);
  done;
  new_code
;;

let clean orig_code =
  let map = compute_map orig_code in
  let new_code = compress orig_code map in
  Step1.remap_code new_code map;
  new_code
;;
