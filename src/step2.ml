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

let is_removable prim bc =
  match bc with
    | Ccall (_, p) -> Prim.no_side_effect prim.(p)
    | Pop _ | Apply _ | Pushtrap _ | Poptrap | Raise | Reraise
    | Raisenotrace | Offsetclosure _ | Offsetref _ | Checksignals
    | Event | Break | Setglobal _ | Setvectitem | Setstringchar
    | Branch _ | Switch (_, _) | Setfield _ | Setfloatfield _
    | Blint (_, _) | Bleint (_, _) | Bgtint (_, _) | Bgeint (_, _)
    | Beq (_, _) | Bneq (_, _) | Bultint (_, _) | Bugeint (_, _)
    | Branchif _ | Branchifnot _ | Divint | Modint | Stop ->
      false
    | _ ->
      true
;;

let compute_deps code =
  let nb_instr = Array.length code in
  let stacks = Array.make nb_instr [||] in
  let accus = Array.make nb_instr [] in
  let stack_sizes = Array.make nb_instr (-1) in
  let use_ind = Array.make nb_instr [] in
  let use = Array.make nb_instr [] in
  let used_by = Array.make nb_instr 0 in
  let in_main = Array.make nb_instr false in
  let factor_list =
    let fact_table = Array.make nb_instr [] in
    fun l -> match l with
      | [] -> []
      | (x :: _) ->
        let rec f l ls ft x = match ls with
          | [] -> ft.(x) <- l :: ft.(x); l
          | l' :: rest -> if l = l' then l' else f l rest ft x in
        f l fact_table.(x) fact_table x in
  let rec pop s n = if n = 0 then s else pop (List.tl s) (pred n) in
  let rec f i s a z =
    let use_stack n =
      if not (List.mem n use_ind.(i)) then use_ind.(i) <- n :: use_ind.(i);
      let c = List.nth s n in
      if not (List.mem c use.(i)) then (
        use.(i) <- c :: use.(i);
        used_by.(c) <- used_by.(c) + 1;
      )
    in
    let use_accu () =
      if not (List.mem a use.(i)) && a <> -1 then (
        use.(i) <- a :: use.(i);
        used_by.(a) <- used_by.(a) + 1;
      )
    in
    let use_stack_top n =
      for k = 0 to n - 1 do use_stack k done;
    in
    let error () = failwith "Step2.compute_deps" in
    let test_stack =
      if stack_sizes.(i) = -1 then
        let sz = List.length s in
        let tbl = Array.make sz [] in
        let rec f i s = match s with
          | [] -> ()
          | n :: rest -> tbl.(i) <- factor_list [ n ]; f (i + 1) rest in
        f 0 s;
        stacks.(i) <- tbl;
        true
      else
        let rec f i s tbl b = match s with
          | [] -> b
          | n :: rest ->
            if List.mem n tbl.(i) then f (i + 1) rest tbl b else
              (tbl.(i) <- factor_list (n :: tbl.(i));
               f (i + 1) rest tbl true) in
        try f 0 s stacks.(i) false with _ -> error ()
    in
    let test_accu =
      if a <> -1 && not (List.mem a accus.(i)) then
        (accus.(i) <- a :: accus.(i); true) else false
    in
    let sz = stack_sizes.(i) in
    if sz = -1 then stack_sizes.(i) <- z else if sz <> z then error ();
    if i < nb_instr && (test_stack || test_accu) then (
      in_main.(i) <- true;
      match code.(i) with
        | Push ->
          use_accu (); f (succ i) (i :: s) a (succ z);
        | Pop n ->
          f (succ i) (pop s n) a (z - n);
        | Assign n ->
          let rec g s k =
            if k = 0 then i :: List.tl s else
              List.hd s :: g (List.tl s) (pred k)
          in
          use_accu (); f (succ i) (g s n) i z;
        | Pushretaddr _ ->
          f (succ i) (i :: i :: i :: s) a (z + 3);
        | Apply n ->
          if n > 3 then (
            use_accu (); use_stack_top (n + 3);
            f (succ i) (pop s (n + 3)) i (z - n - 3);
          ) else (
            use_accu (); use_stack_top n;
            f (succ i) (pop s n) i (z - n);
          )
        | Closure (n, _) ->
          if n > 0 then use_accu (); use_stack_top n;
          f (succ i) (pop s (max (n - 1) 0)) i (z - max (n - 1) 0);
        | Closurerec (cf, cv, _, _) ->
          if cv > 0 then use_accu (); use_stack_top cv;
          let rec g s k = if k = 0 then s else g (i :: s) (pred k) in
          let nb_pop = max (cv - 1) 0 in
          f (succ i) (g (pop s nb_pop) cf) i (z - nb_pop + cf);
        | Makeblock (n, _) | Makefloatblock n ->
          use_accu (); use_stack_top (n - 1);
          f (succ i) (pop s (n - 1)) i (z - n + 1);
        | Pushtrap ptr ->
          f ptr.instr_ind s a z;
          f (succ i) (i :: i :: i :: i :: s) a (z + 4);
        | Poptrap ->
          f (succ i) (pop s 4) a (z - 4);
        | Raise | Reraise | Raisenotrace ->
          use_accu ();
        | Getmethod | Getdynmet ->
          use_accu (); use_stack 0; f (succ i) s i z;
        | Getpubmet (_, _) ->
          use_accu (); f (succ i) (i :: s) i (succ z);
        | Checksignals | Event | Break | Nop ->
          f (succ i) s a z;
        | Getglobalfield (_, _) | Offsetclosure _ | Getglobal _
        | Atom _ | Const _ | Envacc _ ->
          f (succ i) s i z;
        | Offsetref _ ->
          use_accu (); f (succ i) s a z;
        | Getfield _ | Getfloatfield _ | Vectlength | Setglobal _
        | Negint | Boolnot | Offsetint _ | Isint ->
          use_accu (); f (succ i) s i z;
        | Acc n ->
          use_stack n; f (succ i) s i z;
        | Setvectitem | Setstringchar ->
          use_accu (); use_stack 0; use_stack 1;
          f (succ i) (pop s 2) i (z - 2);
        | Branch ptr ->
          f ptr.instr_ind s a z;
        | Switch (_, tab) ->
          use_accu (); Array.iter (fun ptr -> f ptr.instr_ind s a z) tab;
        | Ccall (n, _) ->
          use_accu (); use_stack_top (n - 1);
          f (succ i) (pop s (n - 1)) i (z - n + 1);
        | Addint | Subint | Mulint | Divint | Modint | Andint | Orint | Xorint
        | Lslint | Lsrint | Asrint | Eq | Neq | Ltint | Leint | Gtint | Geint
        | Setfield _ | Setfloatfield _ | Getvectitem | Getstringchar
        | Ultint | Ugeint ->
          use_accu (); use_stack 0; f (succ i) (pop s 1) i (z - 1);
        | Blint (_, ptr) | Bleint (_, ptr) | Bgtint (_, ptr) | Bgeint (_, ptr)
        | Beq (_, ptr) | Bneq (_, ptr) | Bultint (_, ptr) | Bugeint (_, ptr)
        | Branchif ptr | Branchifnot ptr ->
          use_accu (); f ptr.instr_ind s a z; f (succ i) s a z;
        | Return _ | Restart | Grab _ | Appterm (_, _) ->
          error ()
        | Stop -> ()
    )
  in
  f 0 [] (-1) 0;
  (accus, stacks, stack_sizes, in_main, use_ind, use, used_by)
;;

let compute_cleanables code prim in_main use used_by =
  let nb_instr = Array.length code in
  let cleanables = Array.make nb_instr false in
  let rec f i =
    if in_main.(i) && used_by.(i) = 0 && is_removable prim code.(i) then
      let ui = use.(i) in
      use.(i) <- [];
      cleanables.(i) <- true;
      List.iter (fun k -> used_by.(k) <- used_by.(k) - 1) ui;
      List.iter f ui;
  in
  for i = 0 to nb_instr - 1 do f i done;
  cleanables
;;

let compute_blocked code accus stacks in_main use_ind cleanables =
  let nb_instr = Array.length accus in
  let blocked = Array.make nb_instr false in
  let check l =
    if List.exists (fun k -> not cleanables.(k)) l then
      List.iter (fun k -> blocked.(k) <- true) l
  in
  for i = 0 to nb_instr - 1 do
    if in_main.(i) then (
      check accus.(i);
      List.iter
        (fun k -> check stacks.(i).(k))
        use_ind.(i);
      if not cleanables.(i) then
        match code.(i) with
          | Assign n -> List.iter (fun k -> blocked.(k) <- true) stacks.(i).(n)
          | _ -> ()
    )
  done;
  blocked
;;

let clean_code code stacks in_main cleanables blocked =
  let nb_instr = Array.length code in
  let error () = failwith "Step2.clean_code" in
  for i = 0 to nb_instr - 1 do
    let compute_new_stack_top_size n =
      let rec f l k i r =
        if k = 0 then r else
          let n = List.hd l.(i) in
          f l (pred k) (i + 1)
            (if cleanables.(n) && not blocked.(n) then r else r + 1) in
      f stacks.(i) n 0 0
    in
    if in_main.(i) then
      if cleanables.(i) then
        code.(i) <-
          match code.(i) with
            | Push | Pushretaddr _ | Getpubmet (_, _) ->
              if blocked.(i) then code.(i) else Nop
            | Closurerec (cf, cv, _, _) ->
              let nb_pop = max (cv - 1) 0 - cf in
              if nb_pop > 0 then Pop (compute_new_stack_top_size nb_pop)
              else if nb_pop = 0 || not blocked.(i) then Nop
              else code.(i)
            | Nop | Getmethod | Getdynmet | Envacc _ | Atom _ | Getfield _
            | Getfloatfield _ | Vectlength | Getglobal _
            | Getglobalfield (_, _) | Const _ | Boolnot | Negint
            | Offsetint _ | Isint | Acc _  | Assign _ ->
              Nop
            | Addint | Subint | Mulint | Divint | Modint | Andint | Orint
            | Xorint | Lslint | Lsrint | Asrint | Eq | Neq | Ltint | Leint
            | Gtint | Geint | Ultint | Ugeint | Getvectitem
            | Getstringchar ->
              Pop (compute_new_stack_top_size 1)
            | Ccall (n, _) | Makeblock (n, _) | Makefloatblock n ->
              Pop (compute_new_stack_top_size (n - 1))
            | Closure (n, _) ->
              Pop (compute_new_stack_top_size (max (n - 1) 0))
            | Pop _ | Apply _ | Pushtrap _ | Poptrap | Raise | Reraise
            | Raisenotrace | Offsetclosure _ | Offsetref _ | Checksignals
            | Event | Break | Setglobal _ | Setvectitem | Setstringchar
            | Branch _ | Switch (_, _) | Setfield _ | Setfloatfield _
            | Blint (_, _) | Bleint (_, _) | Bgtint (_, _) | Bgeint (_, _)
            | Beq (_, _) | Bneq (_, _) | Bultint (_, _) | Bugeint (_, _)
            | Branchif _ | Branchifnot _ | Stop
            | Appterm (_, _) | Return _ | Restart | Grab _ ->
              error ()
              else (* not cleanable *)
                match code.(i) with
                  | Acc n -> code.(i) <- Acc (compute_new_stack_top_size n)
                  | Pop n -> code.(i) <- Pop (compute_new_stack_top_size n)
                  | _ -> ()
  done;
  ignore (code, cleanables, blocked);
;;

let clean code prim =
  let (accus, stacks, _, in_main, use_ind, use, used_by) = compute_deps code in
  let cleanables = compute_cleanables code prim in_main use used_by in
  let blocked = compute_blocked code accus stacks in_main use_ind cleanables in
  clean_code code stacks in_main cleanables blocked;
;;
