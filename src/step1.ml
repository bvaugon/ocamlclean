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

let nop = POP 0

let remap_instr ptr_map instr =
  let map_ptr ptr = ptr_map.(ptr) in
  match instr with
  | PUSH_RETADDR ptr        -> PUSH_RETADDR (map_ptr ptr)
  | CLOSURE (n, ptr)        -> CLOSURE (n, map_ptr ptr)
  | CLOSUREREC (n, ptrs)    -> CLOSUREREC (n, Array.map map_ptr ptrs)
  | BRANCH ptr              -> BRANCH (map_ptr ptr)
  | BRANCHIF ptr            -> BRANCHIF (map_ptr ptr)
  | BRANCHIFNOT ptr         -> BRANCHIFNOT (map_ptr ptr)
  | PUSHTRAP ptr            -> PUSHTRAP (map_ptr ptr)
  | COMPBRANCH (op, n, ptr) -> COMPBRANCH (op, n, map_ptr ptr)
  | SWITCH (iptrs, pptrs)   -> SWITCH (Array.map map_ptr iptrs, Array.map map_ptr pptrs)
  | _                       -> instr
;;

let remap_code ptr_map new_code =
  Array.map (remap_instr ptr_map) new_code
;;

let compute_cleanables orig_code data =
  let nb_glob = Array.length data in
  let setg_counters = Array.make nb_glob 0 in
  let getg_counters = Array.make nb_glob 0 in
  let getg_fields = Array.make nb_glob [] in
  let setg_ind = Array.make nb_glob 0 in
  let cleanables = Array.make nb_glob None in
  for ind = 0 to Array.length orig_code - 2 do
    match orig_code.(ind), orig_code.(ind + 1) with
    | GETGLOBAL n, GETFIELD p ->
      let l = getg_fields.(n) in
      getg_fields.(n) <- if List.mem p l then l else p :: l
    | GETGLOBAL n, _ ->
      getg_counters.(n) <- succ getg_counters.(n);
    | SETGLOBAL n, _ ->
      setg_counters.(n) <- succ setg_counters.(n);
      setg_ind.(n) <- ind;
    | _, _ ->
      ()
  done;
  for i = 0 to nb_glob - 1 do
    if setg_counters.(i) = 1 && getg_counters.(i) = 0 then
      cleanables.(i) <- Some (setg_ind.(i), getg_fields.(i))
  done;
  cleanables
;;

let compute_branch_targets code =
  let nb_instr = Array.length code in
  let btargs = Array.make nb_instr false in
  Array.iter (fun instr ->
    let ptrs = get_ptrs instr in
    List.iter (fun ptr -> btargs.(ptr) <- true) ptrs;
  ) code;
  btargs
;;

let prepare_code old_code cleanables ignore_fulls =
  let btargs = compute_branch_targets old_code in
  let glob_nb = Array.length cleanables in
  let add_nb = ref 0 in
  let rec f indg adds cleans =
    if indg < glob_nb then
      match cleanables.(indg) with
        | Some (setg_ind, used_fields) ->
          if setg_ind >= 2 && not btargs.(setg_ind) &&
            not btargs.(setg_ind - 1) && not btargs.(setg_ind - 2)
          then
            match (old_code.(setg_ind - 2), old_code.(setg_ind - 1)) with
              | (MAKEBLOCK (_, size), POP popn) ->
                let rec g i =
                  if i >= size then i else
                    let ind = setg_ind - 4 - 2 * i in
                    if ind < 0 || btargs.(ind) || btargs.(ind + 1) then
                      i
                    else
                      match (old_code.(ind), old_code.(ind + 1)) with
                        | (PUSH, ACC _) -> g (i + 1)
                        | _ -> i
                in
                let max = g 0 in
                if List.length used_fields = size && ignore_fulls then
                  f (succ indg) adds cleans
                else if max < size then (
                  old_code.(setg_ind - 1) <- POP (popn + size - max);
                  add_nb := !add_nb + size - max;
                  f (succ indg) ((setg_ind, max, size - max) :: adds)
                    ((setg_ind, used_fields) :: cleans)
                ) else
                  f (succ indg) adds ((setg_ind, used_fields) :: cleans)
              | _ -> f (succ indg) adds cleans
                else
                  f (succ indg) adds cleans
              | None -> f (succ indg) adds cleans
          else
            (List.sort compare adds, cleans)
  in
  let adds, cleans = f 0 [] [] in
  let old_code_size = Array.length old_code in
  let new_code = Array.make (old_code_size + 2 * !add_nb) nop in
  let ptr_map = Array.make old_code_size (-1) in
  let rec g adds_rest i j =
    match adds_rest with
      | (setg_ind, max, add_nb) :: tl ->
        let lim_k = setg_ind - 2 - 2 * max in
        let rec f k l =
          if k = lim_k then
            for n = 0 to add_nb - 1 do
              new_code.(l + 2 * n) <- PUSH;
              new_code.(l + 2 * n + 1) <- ACC (add_nb - 1);
            done
          else (
            new_code.(l) <- old_code.(k);
            ptr_map.(k) <- l;
            f (succ k) (succ l);
          )
        in
        f i j;
        for n = 0 to max - 1 do
          let k = setg_ind - 3 - 2 * n in
          match old_code.(k) with
            | ACC p -> old_code.(k) <- ACC (p + add_nb);
            | _ -> assert false
        done;
        g tl lim_k (j + lim_k - i + 2 * add_nb);
      | [] ->
        let rec f k l =
          if k < old_code_size then (
            new_code.(l) <- old_code.(k);
            ptr_map.(k) <- l;
            f (succ k) (succ l);
          )
        in
        f i j
  in
  g adds 0 0;
  let new_cleans =
    List.map (fun (setg_ind, used_fields) -> (ptr_map.(setg_ind), used_fields))
      cleans
  in
  let new_code = remap_code ptr_map new_code in
  (new_code, new_cleans)
;;

let clean_code new_code data cleans =
  let data_map = Array.make (Array.length data) [||] in
  let update_new_code (setg_ind, used_fields) =
    match new_code.(setg_ind-2), new_code.(setg_ind-1), new_code.(setg_ind) with
      | MAKEBLOCK (mbt, mbs), POP pn, SETGLOBAL sgn ->
        let nb_used_fields = List.length used_fields in
        let cpt = ref 0 in
        data_map.(sgn) <- Array.make mbs (-1);
        List.iter (fun n -> data_map.(sgn).(n) <- 0) used_fields;
        for i = 0 to mbs - 1 do
          if data_map.(sgn).(i) = -1 then (
            new_code.(setg_ind - 4 - 2 * i) <- nop;
            new_code.(setg_ind - 3 - 2 * i) <- nop;
          ) else (
            data_map.(sgn).(i) <- !cpt;
            incr cpt;
          )
        done;
        cpt := 0;
        for i = mbs - 1 downto 0 do
          if data_map.(sgn).(i) = -1 then
            incr cpt
          else
            match new_code.(setg_ind - 3 - 2 * i) with
              | ACC k -> new_code.(setg_ind - 3 - 2 * i) <- ACC (k - !cpt)
              | _ -> assert false
        done;
        if nb_used_fields <> 0 then
          new_code.(setg_ind-2) <- MAKEBLOCK (mbt, nb_used_fields)
        else
          new_code.(setg_ind-2) <- ATOM mbt;
        if nb_used_fields = 0 then new_code.(setg_ind-1) <- POP (pn - 1);
      | _ -> assert false
  in
  List.iter update_new_code cleans;
  for i = 0 to Array.length new_code - 2 do
    match new_code.(i), new_code.(i + 1) with
    | GETGLOBAL n, GETFIELD p ->
      if data_map.(n) <> [||] then new_code.(i + 1) <- GETFIELD data_map.(n).(p);
    | _ ->
      ()
  done
;;

let clean old_code data =
  let cleanables = compute_cleanables old_code data in
  let (new_code, cleans) = prepare_code old_code cleanables true in
  clean_code new_code data cleans;
  new_code
;;
