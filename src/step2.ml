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

let is_removable prim bc =
  match bc with
    | C_CALL (_, p) ->
      Prim.no_side_effect prim.(p)
    |  POP _ | APPLY _ | PUSHTRAP _ | POPTRAP | RAISE | RERAISE
    | RAISE_NOTRACE | OFFSETCLOSURE _ | OFFSETREF _ | CHECK_SIGNALS
    | SETGLOBAL _ | SETVECTITEM | SETBYTESCHAR
    | BRANCH _ | BRANCHIF _ | BRANCHIFNOT _ 
    | SWITCH (_, _) | SETFIELD _ | SETFLOATFIELD _
    | COMPBRANCH (_, _, _) | BINAPP (DIV | MOD) | STOP ->
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
        | PUSH ->
          use_accu (); f (succ i) (i :: s) a (succ z);
        | POP n ->
          f (succ i) (pop s n) a (z - n);
        | ASSIGN n ->
          let rec g s k =
            if k = 0 then i :: List.tl s else
              List.hd s :: g (List.tl s) (pred k)
          in
          use_accu (); f (succ i) (g s n) i z;
        | PUSH_RETADDR _ ->
          f (succ i) (i :: i :: i :: s) a (z + 3);
        | APPLY n ->
          if n > 3 then (
            use_accu (); use_stack_top (n + 3);
            f (succ i) (pop s (n + 3)) i (z - n - 3);
          ) else (
            use_accu (); use_stack_top n;
            f (succ i) (pop s n) i (z - n);
          )
        | CLOSURE (n, _) ->
          if n > 0 then use_accu (); use_stack_top n;
          f (succ i) (pop s (max (n - 1) 0)) i (z - max (n - 1) 0);
        | CLOSUREREC (cv, ct) ->
          let cf = Array.length ct in
          if cv > 0 then use_accu (); use_stack_top cv;
          let rec g s k = if k = 0 then s else g (i :: s) (pred k) in
          let nb_pop = max (cv - 1) 0 in
          f (succ i) (g (pop s nb_pop) cf) i (z - nb_pop + cf);
        | MAKEBLOCK (_, n) | MAKEFLOATBLOCK n ->
          use_accu (); use_stack_top (n - 1);
          f (succ i) (pop s (n - 1)) i (z - n + 1);
        | PUSHTRAP ptr ->
          f ptr s a z;
          f (succ i) (i :: i :: i :: i :: s) a (z + 4);
        | POPTRAP ->
          f (succ i) (pop s 4) a (z - 4);
        | RAISE | RERAISE | RAISE_NOTRACE ->
          use_accu ();
        | GETMETHOD | GETDYNMET ->
          use_accu (); use_stack 0; f (succ i) s i z;
        | GETPUBMET _ ->
          use_accu (); f (succ i) (i :: s) i (succ z);
        | CHECK_SIGNALS ->
          f (succ i) s a z;
        | OFFSETCLOSURE _ | GETGLOBAL _
        | ATOM _ | CONSTINT _ | ENVACC _ ->
          f (succ i) s i z;
        | OFFSETREF _ ->
          use_accu (); f (succ i) s a z;
        | GETFIELD _ | GETFLOATFIELD _ | SETGLOBAL _
        | UNAPP _ ->
          use_accu (); f (succ i) s i z;
        | ACC n ->
          use_stack n; f (succ i) s i z;
        | SETVECTITEM | SETBYTESCHAR ->
          use_accu (); use_stack 0; use_stack 1;
          f (succ i) (pop s 2) i (z - 2);
        | BRANCH ptr ->
          f ptr s a z;
        | SWITCH (iptrs, pptrs) ->
          use_accu ();
          Array.iter (fun ptr -> f ptr s a z) iptrs;
          Array.iter (fun ptr -> f ptr s a z) pptrs;
        | C_CALL (n, _) ->
          use_accu (); use_stack_top (n - 1);
          f (succ i) (pop s (n - 1)) i (z - n + 1);
        | BINAPP _ | COMPARE _
        | SETFIELD _ | SETFLOATFIELD _ | GETVECTITEM
        | GETBYTESCHAR | GETSTRINGCHAR ->
          use_accu (); use_stack 0; f (succ i) (pop s 1) i (z - 1);
        | COMPBRANCH (_, _, ptr) | BRANCHIF ptr | BRANCHIFNOT ptr ->
          use_accu (); f ptr s a z; f (succ i) s a z;
        | RETURN _ | RESTART | GRAB _ | APPTERM (_, _) ->
          error ()
        | STOP -> ()
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
          | ASSIGN n -> List.iter (fun k -> blocked.(k) <- true) stacks.(i).(n)
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
            | PUSH | PUSH_RETADDR _ | GETPUBMET _ ->
              if blocked.(i) then code.(i) else Step1.nop
            | CLOSUREREC (cv, ct) ->
              let cf = Array.length ct in
              let nb_pop = max (cv - 1) 0 - cf in
              if nb_pop > 0 then POP (compute_new_stack_top_size nb_pop)
              else if nb_pop = 0 || not blocked.(i) then Step1.nop
              else code.(i)
            | GETMETHOD | GETDYNMET | ENVACC _ | ATOM _
            | GETFIELD _ | GETFLOATFIELD _ | GETGLOBAL _
            | CONSTINT _ | UNAPP _ | ACC _ | ASSIGN _ ->
              Step1.nop
            | BINAPP _ | COMPARE _
            | GETVECTITEM | GETSTRINGCHAR | GETBYTESCHAR ->
              POP (compute_new_stack_top_size 1)
            | C_CALL (n, _) | MAKEBLOCK (_, n) | MAKEFLOATBLOCK n ->
              POP (compute_new_stack_top_size (n - 1))
            | CLOSURE (n, _) ->
              POP (compute_new_stack_top_size (max (n - 1) 0))
            | POP _ | APPLY _ | PUSHTRAP _ | POPTRAP
            | RAISE | RERAISE | RAISE_NOTRACE | OFFSETCLOSURE _
            | OFFSETREF _ | CHECK_SIGNALS
            | SETGLOBAL _ | SETVECTITEM | SETBYTESCHAR
            | BRANCH _ | SWITCH (_, _) | SETFIELD _ | SETFLOATFIELD _
            | COMPBRANCH (_, _, _) | BRANCHIF _ | BRANCHIFNOT _ | STOP
            | APPTERM (_, _) | RETURN _ | RESTART | GRAB _ ->
              error ()
              else (* not cleanable *)
                match code.(i) with
                  | ACC n -> code.(i) <- ACC (compute_new_stack_top_size n)
                  | POP n -> code.(i) <- POP (compute_new_stack_top_size n)
                  | _ -> ()
  done;
;;

let clean code prim =
  let (accus, stacks, _, in_main, use_ind, use, used_by) = compute_deps code in
  let cleanables = compute_cleanables code prim in_main use used_by in
  let blocked = compute_blocked code accus stacks in_main use_ind cleanables in
  clean_code code stacks in_main cleanables blocked;
;;
