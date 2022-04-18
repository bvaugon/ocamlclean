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

let compute_map orig_code =
  let nb_instr = Array.length orig_code in
  let map = Array.make nb_instr 0 in
  let rec f i j =
    if i < nb_instr then (
      map.(i) <- j;
      if orig_code.(i) = Step1.nop
      then f (succ i) j
      else f (succ i) (succ j)
    )
  in
  f 0 0;
  map
;;

let compress orig_code map =
  let orig_size = Array.length orig_code in
  let new_size = map.(Array.length map - 1) + 1 in
  let new_code = Array.make new_size Step1.nop in
  for i = 0 to orig_size - 1 do
    if orig_code.(i) <> Step1.nop
    then new_code.(map.(i)) <- orig_code.(i);
  done;
  new_code
;;

let clean orig_code =
  let map = compute_map orig_code in
  let new_code = compress orig_code map in
  Array.map (Step1.remap_instr map) new_code
;;
