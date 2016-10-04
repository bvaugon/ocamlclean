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

exception Exn of string

let compute_ptrs code =
  let nb_instr = Array.length code in
  let nb_bc = code.(nb_instr - 1).old_addr + 1 in
  let indirect = Array.make nb_bc None in
  let grep_instr instr_ind instr =
    indirect.(instr.old_addr) <- Some instr_ind;
  in
  let search bc_ind =
    if bc_ind < 0 || bc_ind >= nb_bc then (raise (Exn "invalid offset"));
    match indirect.(bc_ind) with
      | None -> raise (Exn "invalid offset");
      | Some instr_ind -> instr_ind
  in
  let affect_ptr instr =
    let update_pointed delta ptr =
      let instr_ind = search (instr.old_addr + delta + ptr.old_ofs) in
      ptr.instr_ind <- instr_ind;
    in
    match instr.bc with
      | Pushretaddr ptr | Branch ptr | Branchif ptr | Branchifnot ptr
      | Pushtrap ptr ->
        update_pointed 1 ptr;

      | Closure (_, ptr) | Beq (_, ptr) | Bneq (_, ptr) | Blint (_, ptr)
      | Bleint (_, ptr) | Bgtint (_, ptr) | Bgeint (_, ptr) | Bultint (_, ptr)
      | Bugeint (_, ptr) ->
        update_pointed 2 ptr;

      | Closurerec (_, _, ptr, tab) ->
        update_pointed 3 ptr;
        Array.iter (update_pointed 3) tab;

      | Switch (_, tab) ->
        Array.iter (update_pointed 2) tab

      | _ -> ();
  in
  for i = pred nb_instr downto 0 do grep_instr i code.(i) done;
  Array.iter affect_ptr code;
;;

let parse ic index =
  let (offset, length) =
    try
      Index.find_section index Index.Code
    with Not_found ->
      raise (Exn "code section not found")
  in
  seek_in ic offset;
  let cpt = ref 0 in
  let nb_bc = length lsr 2 in
  let read =
    let buf4 = Bytes.create 4 in
    fun () ->
      incr cpt;
      if !cpt > nb_bc then raise End_of_file;
      really_input ic buf4 0 4;
      let res =
        (int_of_char (Bytes.get buf4 0)) lor (int_of_char (Bytes.get buf4 1) lsl 8) lor
          (int_of_char (Bytes.get buf4 2) lsl 16) lor (int_of_char (Bytes.get buf4 3) lsl 24)
      in
      match Sys.word_size with
        | 32 -> res
        | 64 -> (res lsl 32) asr 32
        | ws -> raise (Exn (Printf.sprintf "Unsupported architecture: word size is %d" ws))
  in
  let rec f i acc =
    let old_addr = !cpt in
    match Instr.parse read with
      | None -> acc
      | Some bcs ->
        let instrs =
          List.map (fun bc -> { old_addr = old_addr ; bc = bc }) bcs
        in
        f (i + 1) (List.rev instrs @ acc)
  in
  let code = Array.of_list (f 0 []) in
  let nb_instr = Array.length code in
  for i = 0 to nb_instr / 2 - 1 do
    let s = nb_instr - i - 1 in
    let tmp = code.(i) in
    code.(i) <- code.(s);
    code.(s) <- tmp;
  done;
  compute_ptrs code;
  Array.map (fun { bc = bc ; old_addr = _ } -> bc) code
;;

let print oc code =
  let f ind bc = Printf.fprintf oc "%-8d %a\n" ind print_bc bc in
  Array.iteri f code;
;;

let compute_branch_targets code =
  let nb_instr = Array.length code in
  let btargs = Array.make nb_instr false in
  let f bc =
    let register ptr = btargs.(ptr.instr_ind) <- true in
    match bc with
      | Pushretaddr ptr | Closure (_, ptr) | Branch ptr | Branchif ptr
      | Branchifnot ptr | Pushtrap ptr | Beq (_,ptr) | Bneq (_,ptr)
      | Blint (_,ptr) | Bleint (_,ptr) | Bgtint (_,ptr) | Bgeint (_,ptr)
      | Bultint (_,ptr) | Bugeint (_,ptr) ->
        register ptr;
      | Switch (_, tab) -> Array.iter register tab;
      | Closurerec (_, _, ptr, tab) -> register ptr; Array.iter register tab;
      | _ -> ()
  in
  Array.iter f code;
  btargs
;;

let export oc code =
  let nb_instr = Array.length code in
  let btargs = compute_branch_targets code in
  let map = Array.make nb_instr (-1) in
  let pass write step =
    let rec normal i =
      if i < nb_instr then
        let write_ptr delta ptr =
          write (map.(ptr.instr_ind) - map.(i) - delta)
        in
        let write_ptrs delta tab = Array.iter (write_ptr delta) tab in
        let bc = code.(i) in
        step i;
        match bc with
          | Push -> after_push (succ i)
          | Pop 0 | Nop -> normal (succ i)
          | Pop n -> after_pop (succ i) n
          | _ ->
            begin match bc with
              | Push | Pop _ | Nop    -> assert false
              | Acc 0                 -> write 0
              | Acc 1                 -> write 1
              | Acc 2                 -> write 2
              | Acc 3                 -> write 3
              | Acc 4                 -> write 4
              | Acc 5                 -> write 5
              | Acc 6                 -> write 6
              | Acc 7                 -> write 7
              | Acc n                 -> write 8 ; write n
              | Assign n              -> write 20 ; write n
              | Envacc 1              -> write 21
              | Envacc 2              -> write 22
              | Envacc 3              -> write 23
              | Envacc 4              -> write 24
              | Envacc n              -> write 25 ; write n
              | Pushretaddr ptr       -> write 31 ; write_ptr 1 ptr
              | Apply 1               -> write 33
              | Apply 2               -> write 34
              | Apply 3               -> write 35
              | Apply n               -> write 32 ; write n
              | Appterm (n, 1)        -> write 37 ; write n
              | Appterm (n, 2)        -> write 38 ; write n
              | Appterm (n, 3)        -> write 39 ; write n
              | Appterm (n, s)        -> write 36 ; write n ; write s
              | Return n              -> write 40 ; write n
              | Restart               -> write 41
              | Grab n                -> write 42 ; write n
              | Closure (n, ptr)      -> write 43 ; write n ; write_ptr 2 ptr
              | Closurerec (f, v, o, t) ->
                write 44 ; write f ; write v;
                write_ptr 3 o ; write_ptrs 3 t;
              | Offsetclosure (-2)    -> write 45
              | Offsetclosure 0       -> write 46
              | Offsetclosure 2       -> write 47
              | Offsetclosure n       -> write 48 ; write n
              | Getglobal n           -> write 53 ; write n
              | Getglobalfield (n, p) -> write 55 ; write n ; write p
              | Setglobal n           -> write 57 ; write n
              | Makeblock (0, 0) | Atom 0 -> write 58
              | Makeblock (0, t) | Atom t -> write 59 ; write t
              | Makeblock (1, t)      -> write 63 ; write t
              | Makeblock (2, t)      -> write 64 ; write t
              | Makeblock (3, t)      -> write 65 ; write t
              | Makeblock (n, t)      -> write 62 ; write n ; write t
              | Makefloatblock n      -> write 66 ; write n
              | Getfield 0            -> write 67
              | Getfield 1            -> write 68
              | Getfield 2            -> write 69
              | Getfield 3            -> write 70
              | Getfield n            -> write 71 ; write n
              | Getfloatfield n       -> write 72 ; write n
              | Setfield 0            -> write 73
              | Setfield 1            -> write 74
              | Setfield 2            -> write 75
              | Setfield 3            -> write 76
              | Setfield n            -> write 77 ; write n
              | Setfloatfield n       -> write 78 ; write n
              | Vectlength            -> write 79
              | Getvectitem           -> write 80
              | Setvectitem           -> write 81
              | Getstringchar         -> write 82
              | Setstringchar         -> write 83
              | Branch ptr            -> write 84 ; write_ptr 1 ptr
              | Branchif ptr          -> write 85 ; write_ptr 1 ptr
              | Branchifnot ptr       -> write 86 ; write_ptr 1 ptr
              | Switch (n, tab)       -> write 87 ; write n ; write_ptrs 2 tab
              | Boolnot               -> write 88
              | Pushtrap ptr          -> write 89 ; write_ptr 1 ptr
              | Poptrap               -> write 90
              | Raise                 -> write 91
              | Checksignals          -> write 92
              | Ccall (1, p)          -> write 93 ; write p
              | Ccall (2, p)          -> write 94 ; write p
              | Ccall (3, p)          -> write 95 ; write p
              | Ccall (4, p)          -> write 96 ; write p
              | Ccall (5, p)          -> write 97 ; write p
              | Ccall (n, p)          -> write 98 ; write n ; write p
              | Const 0               -> write 99
              | Const 1               -> write 100
              | Const 2               -> write 101
              | Const 3               -> write 102
              | Const n               -> write 103 ; write n
              | Negint                -> write 109
              | Addint                -> write 110
              | Subint                -> write 111
              | Mulint                -> write 112
              | Divint                -> write 113
              | Modint                -> write 114
              | Andint                -> write 115
              | Orint                 -> write 116
              | Xorint                -> write 117
              | Lslint                -> write 118
              | Lsrint                -> write 119
              | Asrint                -> write 120
              | Eq                    -> write 121
              | Neq                   -> write 122
              | Ltint                 -> write 123
              | Leint                 -> write 124
              | Gtint                 -> write 125
              | Geint                 -> write 126
              | Offsetint ofs         -> write 127 ; write ofs
              | Offsetref ofs         -> write 128 ; write ofs
              | Isint                 -> write 129
              | Getmethod             -> write 130
              | Beq (0, ptr)          -> write 86 ; write_ptr 1 ptr
              | Bneq (0, ptr)         -> write 85 ; write_ptr 1 ptr
              | Beq (v, ptr)          -> write 131 ; write v ; write_ptr 2 ptr
              | Bneq (v, ptr)         -> write 132 ; write v ; write_ptr 2 ptr
              | Blint (v, ptr)        -> write 133 ; write v ; write_ptr 2 ptr
              | Bleint (v, ptr)       -> write 134 ; write v ; write_ptr 2 ptr
              | Bgtint (v, ptr)       -> write 135 ; write v ; write_ptr 2 ptr
              | Bgeint (v, ptr)       -> write 136 ; write v ; write_ptr 2 ptr
              | Ultint                -> write 137
              | Ugeint                -> write 138
              | Bultint (v, ptr)      -> write 139 ; write v ; write_ptr 2 ptr
              | Bugeint (v, ptr)      -> write 140 ; write v ; write_ptr 2 ptr
              | Getpubmet (v, cch)    -> write 141 ; write v ; write cch
              | Getdynmet             -> write 142
              | Stop                  -> write 143
              | Event                 -> write 144
              | Break                 -> write 145
              | Reraise               -> write 146
              | Raisenotrace          -> write 147
            end;
            normal (succ i);
    and after_pop i acc =
      if i < nb_instr then (
        if not btargs.(i) then (
          match code.(i) with
            | Pop n -> after_pop (succ i) (acc + n)
            | Nop -> after_pop (succ i) acc
            | _ -> write 19 ; write acc ; normal i
        ) else (
          write 19 ; write acc ; normal i
        )
      ) else (
        write 19 ; write acc
      )
    and after_push i =
      if i < nb_instr then (
        if not btargs.(i) then (
          match code.(i) with
            | Nop | Pop 0 -> step i ; after_push (succ i)
            | Acc 0 -> write 10 ; normal (succ i)
            | Acc 1 -> write 11 ; normal (succ i)
            | Acc 2 -> write 12 ; normal (succ i)
            | Acc 3 -> write 13 ; normal (succ i)
            | Acc 4 -> write 14 ; normal (succ i)
            | Acc 5 -> write 15 ; normal (succ i)
            | Acc 6 -> write 16 ; normal (succ i)
            | Acc 7 -> write 17 ; normal (succ i)
            | Acc n -> write 18 ; write n ; normal (succ i)
            | Envacc 1 -> write 26 ; normal (succ i)
            | Envacc 2 -> write 27 ; normal (succ i)
            | Envacc 3 -> write 28 ; normal (succ i)
            | Envacc 4 -> write 29 ; normal (succ i)
            | Envacc n -> write 30 ; write n ; normal (succ i)
            | Offsetclosure (-2) -> write 49 ; normal (succ i)
            | Offsetclosure 0 -> write 50 ; normal (succ i)
            | Offsetclosure 2 -> write 51 ; normal (succ i)
            | Offsetclosure n -> write 52 ; write n ; normal (succ i)
            | Getglobal n -> write 54 ; write n ; normal (succ i)
            | Getglobalfield (n,p) ->
              write 56 ; write n ; write p ; normal (succ i)
            | Makeblock (0, 0) | Atom 0 -> write 60 ; normal (succ i)
            | Makeblock (0, t) | Atom t -> write 61 ; write t ; normal (succ i)
            | Const 0 -> write 104 ; normal (succ i)
            | Const 1 -> write 105 ; normal (succ i)
            | Const 2 -> write 106 ; normal (succ i)
            | Const 3 -> write 107 ; normal (succ i)
            | Const n -> write 108 ; write n ; normal (succ i)
            | _ -> write 9 ; normal i
        ) else (
          write 9 ; normal i
        )
      ) else (
        write 9
      )
    in
    normal 0;
  in
  begin
    let cpt = ref 0 in
    let write _ = incr cpt in
    let step i = map.(i) <- !cpt in
    pass write step;
  end;
  begin
    let write n =
      output_byte oc (n land 0xFF);
      output_byte oc ((n asr 8) land 0xFF);
      output_byte oc ((n asr 16) land 0xFF);
      output_byte oc ((n asr 24) land 0xFF);
    in
    let step _ = () in
    pass write step;
  end;
;;
