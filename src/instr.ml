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

exception Exn of string

type instr = { old_addr : int ; bc : bc }
and ptr = { old_ofs : int ; mutable instr_ind : int }
and bc =
  | Acc of int
  | Push
  | Pop of int
  | Assign of int
  | Envacc of int
  | Pushretaddr of ptr
  | Apply of int
  | Appterm of int * int
  | Return of int
  | Restart
  | Grab of int
  | Closure of int * ptr
  | Closurerec of int * int * ptr * ptr array
  | Offsetclosure of int
  | Getglobal of int
  | Getglobalfield of int * int
  | Setglobal of int
  | Atom of int
  | Makeblock of int * int
  | Makefloatblock of int
  | Getfield of int
  | Getfloatfield of int
  | Setfield of int
  | Setfloatfield of int
  | Vectlength
  | Getvectitem
  | Setvectitem
  | Getbyteschar
  | Setbyteschar
  | Branch of ptr
  | Branchif of ptr
  | Branchifnot of ptr
  | Switch of int * ptr array
  | Boolnot
  | Pushtrap of ptr
  | Poptrap
  | Raise
  | Checksignals
  | Ccall of int * int
  | Const of int
  | Negint
  | Addint
  | Subint
  | Mulint
  | Divint
  | Modint
  | Andint
  | Orint
  | Xorint
  | Lslint
  | Lsrint
  | Asrint
  | Eq
  | Neq
  | Ltint
  | Leint
  | Gtint
  | Geint
  | Offsetint of int
  | Offsetref of int
  | Isint
  | Getmethod
  | Beq of int * ptr
  | Bneq of int * ptr
  | Blint of int * ptr
  | Bleint of int * ptr
  | Bgtint of int * ptr
  | Bgeint of int * ptr
  | Ultint
  | Ugeint
  | Bultint of int * ptr
  | Bugeint of int * ptr
  | Getpubmet of int * int
  | Getdynmet
  | Stop
  | Event
  | Break
  | Reraise
  | Raisenotrace
  | Getstringchar
  | Nop
;;

let parse read =
  let read_ptr () = { old_ofs = read () ; instr_ind = -1 } in
  try
    let opcode = read () in
    let instrs =
      try match opcode with
        | 0 ->   [ Acc 0 ]
        | 1 ->   [ Acc 1 ]
        | 2 ->   [ Acc 2 ]
        | 3 ->   [ Acc 3 ]
        | 4 ->   [ Acc 4 ]
        | 5 ->   [ Acc 5 ]
        | 6 ->   [ Acc 6 ]
        | 7 ->   [ Acc 7 ]
        | 8 ->   [ Acc (read ()) ]
        | 9 ->   [ Push ]
        | 10 ->  [ Push ; Acc 0 ]
        | 11 ->  [ Push ; Acc 1 ]
        | 12 ->  [ Push ; Acc 2 ]
        | 13 ->  [ Push ; Acc 3 ]
        | 14 ->  [ Push ; Acc 4 ]
        | 15 ->  [ Push ; Acc 5 ]
        | 16 ->  [ Push ; Acc 6 ]
        | 17 ->  [ Push ; Acc 7 ]
        | 18 ->  [ Push ; Acc (read ()) ]
        | 19 ->  [ Pop (read ()) ]
        | 20 ->  [ Assign (read ()) ]
        | 21 ->  [ Envacc 1 ]
        | 22 ->  [ Envacc 2 ]
        | 23 ->  [ Envacc 3 ]
        | 24 ->  [ Envacc 4 ]
        | 25 ->  [ Envacc (read ()) ]
        | 26 ->  [ Push ; Envacc 1 ]
        | 27 ->  [ Push ; Envacc 2 ]
        | 28 ->  [ Push ; Envacc 3 ]
        | 29 ->  [ Push ; Envacc 4 ]
        | 30 ->  [ Push ; Envacc (read ()) ]
        | 31 ->  [ Pushretaddr (read_ptr ()) ]
        | 32 ->  [ Apply (read ()) ]
        | 33 ->  [ Apply 1 ]
        | 34 ->  [ Apply 2 ]
        | 35 ->  [ Apply 3 ]
        | 36 ->  let n = read () in let s = read () in [ Appterm (n, s) ]
        | 37 ->  [ Appterm ((read ()), 1) ]
        | 38 ->  [ Appterm ((read ()), 2) ]
        | 39 ->  [ Appterm ((read ()), 3) ]
        | 40 ->  [ Return (read ()) ]
        | 41 ->  [ Restart ]
        | 42 ->  [ Grab (read ()) ]
        | 43 ->
          let n = read () in let ptr = read_ptr () in [ Closure (n, ptr) ]
        | 44 ->
          let f = read () in
          let v = read () in
          let o = read_ptr () in
          let t = if f = 1 then [||] else
              let t = Array.make (f - 1) (read_ptr ()) in
              for i = 1 to f - 2 do t.(i) <- read_ptr () done ; t
          in
          [ Closurerec (f, v, o, t) ]
        | 45 ->  [ Offsetclosure (-2) ]
        | 46 ->  [ Offsetclosure 0 ]
        | 47 ->  [ Offsetclosure 2 ]
        | 48 ->  [ Offsetclosure (read ()) ]
        | 49 ->  [ Push ; Offsetclosure (-2) ]
        | 50 ->  [ Push ; Offsetclosure 0 ]
        | 51 ->  [ Push ; Offsetclosure 2 ]
        | 52 ->  [ Push ; Offsetclosure (read ()) ]
        | 53 ->  [ Getglobal (read ()) ]
        | 54 ->  [ Push ; Getglobal (read ()) ]
        | 55 ->  let n = read () in let p = read () in
                                    [ Getglobalfield (n, p) ]
        | 56 ->  let n = read () in let p = read () in
                                    [ Push ; Getglobalfield (n, p) ]
        | 57 ->  [ Setglobal (read ()) ]
        | 58 ->  [ Atom 0 ]
        | 59 ->  [ Atom (read ()) ]
        | 60 ->  [ Push ; Atom 0 ]
        | 61 ->  [ Push ; Atom (read ()) ]
        | 62 ->  let n = read () in let t = read () in [ Makeblock (n, t) ]
        | 63 ->  [ Makeblock (1, read ()) ]
        | 64 ->  [ Makeblock (2, read ()) ]
        | 65 ->  [ Makeblock (3, read ()) ]
        | 66 ->  [ Makefloatblock (read ()) ]
        | 67 ->  [ Getfield 0 ]
        | 68 ->  [ Getfield 1 ]
        | 69 ->  [ Getfield 2 ]
        | 70 ->  [ Getfield 3 ]
        | 71 ->  [ Getfield (read ()) ]
        | 72 ->  [ Getfloatfield (read ()) ]
        | 73 ->  [ Setfield 0 ]
        | 74 ->  [ Setfield 1 ]
        | 75 ->  [ Setfield 2 ]
        | 76 ->  [ Setfield 3 ]
        | 77 ->  [ Setfield (read ()) ]
        | 78 ->  [ Setfloatfield (read ()) ]
        | 79 ->  [ Vectlength ]
        | 80 ->  [ Getvectitem ]
        | 81 ->  [ Setvectitem ]
        | 82 ->  [ Getbyteschar ]
        | 83 ->  [ Setbyteschar ]
        | 84 ->  [ Branch (read_ptr ()) ]
        | 85 ->  [ Branchif (read_ptr ()) ]
        | 86 ->  [ Branchifnot (read_ptr ()) ]
        | 87 ->
          let n = read () in
          let size_tag = n lsr 16 in
          let size_long = n land (1 lsl 16 - 1) in
          let size = size_tag + size_long in
          let tab = Array.init size (fun _ -> read_ptr ()) in
          [ Switch (n, tab) ]
        | 88 ->  [ Boolnot ]
        | 89 ->  [ Pushtrap (read_ptr ()) ]
        | 90 ->  [ Poptrap ]
        | 91 ->  [ Raise ]
        | 92 ->  [ Checksignals ]
        | 93 ->  [ Ccall (1, read ()) ]
        | 94 ->  [ Ccall (2, read ()) ]
        | 95 ->  [ Ccall (3, read ()) ]
        | 96 ->  [ Ccall (4, read ()) ]
        | 97 ->  [ Ccall (5, read ()) ]
        | 98 ->  let n = read () in let p = read () in [ Ccall (n, p) ]
        | 99 ->  [ Const 0 ]
        | 100 -> [ Const 1 ]
        | 101 -> [ Const 2 ]
        | 102 -> [ Const 3 ]
        | 103 -> [ Const (read ()) ]
        | 104 -> [ Push ; Const 0 ]
        | 105 -> [ Push ; Const 1 ]
        | 106 -> [ Push ; Const 2 ]
        | 107 -> [ Push ; Const 3 ]
        | 108 -> [ Push ; Const (read ()) ]
        | 109 -> [ Negint ]
        | 110 -> [ Addint ]
        | 111 -> [ Subint ]
        | 112 -> [ Mulint ]
        | 113 -> [ Divint ]
        | 114 -> [ Modint ]
        | 115 -> [ Andint ]
        | 116 -> [ Orint ]
        | 117 -> [ Xorint ]
        | 118 -> [ Lslint ]
        | 119 -> [ Lsrint ]
        | 120 -> [ Asrint ]
        | 121 -> [ Eq ]
        | 122 -> [ Neq ]
        | 123 -> [ Ltint ]
        | 124 -> [ Leint ]
        | 125 -> [ Gtint ]
        | 126 -> [ Geint ]
        | 127 -> [ Offsetint (read ()) ]
        | 128 -> [ Offsetref (read ()) ]
        | 129 -> [ Isint ]
        | 130 -> [ Getmethod ]
        | 131 -> let v = read () in let ptr = read_ptr () in [ Beq (v, ptr) ]
        | 132 -> let v = read () in let ptr = read_ptr () in [ Bneq (v, ptr) ]
        | 133 -> let v = read () in let ptr = read_ptr () in [ Blint (v, ptr) ]
        | 134 -> let v = read () in let ptr = read_ptr () in [ Bleint (v, ptr) ]
        | 135 -> let v = read () in let ptr = read_ptr () in [ Bgtint (v, ptr) ]
        | 136 -> let v = read () in let ptr = read_ptr () in [ Bgeint (v, ptr) ]
        | 137 -> [ Ultint ]
        | 138 -> [ Ugeint ]
        | 139 -> let v = read () in let ptr = read_ptr () in [ Bultint (v, ptr) ]
        | 140 -> let v = read () in let ptr = read_ptr () in [ Bugeint (v, ptr) ]
        | 141 -> let v = read () in let ofs = read () in [ Getpubmet (v, ofs) ]
        | 142 -> [ Getdynmet ]
        | 143 -> [ Stop ]
        | 144 -> [ Event ]
        | 145 -> [ Break ]
        | 146 -> [ Reraise ]
        | 147 -> [ Raisenotrace ]
        | 148 -> [ Getstringchar ]
        | _ -> raise (Exn (Printf.sprintf "invalid opcode: %d" opcode))
      with End_of_file -> raise (Exn "unexpected end of code section")
    in
    Some instrs
  with End_of_file -> None
;;

let print_bc oc bc =
  match bc with
    | Acc n             -> Printf.fprintf oc "ACC %d" n
    | Push              -> Printf.fprintf oc "PUSH"
    | Pop n             -> Printf.fprintf oc "POP %d" n
    | Assign n          -> Printf.fprintf oc "ASSIGN %d" n
    | Envacc n          -> Printf.fprintf oc "ENVACC %d" n
    | Pushretaddr ptr   -> Printf.fprintf oc "PUSHRETADDR %d" ptr.instr_ind
    | Apply n           -> Printf.fprintf oc "APPLY %d" n
    | Appterm (n,s)     -> Printf.fprintf oc "APPTERM %d %d" n s
    | Return n          -> Printf.fprintf oc "RETURN %d" n
    | Restart           -> Printf.fprintf oc "RESTART"
    | Grab n            -> Printf.fprintf oc "GRAB %d" n
    | Closure (n,ptr)   -> Printf.fprintf oc "CLOSURE %d %d" n ptr.instr_ind
    | Closurerec (f,v,{ instr_ind = o ; old_ofs = _ },t) ->
      Printf.fprintf oc "CLOSUREREC %d %d %d [" f v o;
      Array.iter (fun p -> Printf.fprintf oc " %d " p.instr_ind) t;
      Printf.fprintf oc "]";
    | Offsetclosure n       -> Printf.fprintf oc "OFFSETCLOSURE %d" n
    | Getglobal n           -> Printf.fprintf oc "GETGLOBAL %d" n
    | Getglobalfield (n,p)  -> Printf.fprintf oc "GETGLOBALFIELD %d %d"n p
    | Setglobal n       -> Printf.fprintf oc "SETGLOBAL %d" n
    | Atom n            -> Printf.fprintf oc "ATOM %d" n
    | Makeblock (n,t)   -> Printf.fprintf oc "MAKEBLOCK %d %d" n t
    | Makefloatblock n  -> Printf.fprintf oc "MAKEFLOATBLOCK %d" n
    | Getfield n        -> Printf.fprintf oc "GETFIELD %d" n
    | Getfloatfield n   -> Printf.fprintf oc "GETFLOATFIELD %d" n
    | Setfield n        -> Printf.fprintf oc "SETFIELD %d" n
    | Setfloatfield n   -> Printf.fprintf oc "SETFLOATFIELD %d" n
    | Vectlength        -> Printf.fprintf oc "VECTLENGTH"
    | Getvectitem       -> Printf.fprintf oc "GETVECTITEM"
    | Setvectitem       -> Printf.fprintf oc "SETVECTITEM"
    | Getbyteschar      -> Printf.fprintf oc "GETBYTESCHAR"
    | Setbyteschar      -> Printf.fprintf oc "SETBYTESCHAR"
    | Branch ptr        -> Printf.fprintf oc "BRANCH %d" ptr.instr_ind
    | Branchif ptr      -> Printf.fprintf oc "BRANCHIF %d" ptr.instr_ind
    | Branchifnot ptr   -> Printf.fprintf oc "BRANCHIFNOT %d" ptr.instr_ind
    | Switch (n, tab) ->
      let size_tag = n lsr 16 in
      let size_long = n land 0xFFFF in
      Printf.fprintf oc "SWITCH %d %d [" size_tag size_long;
      Array.iter (fun ptr -> Printf.fprintf oc " %d " ptr.instr_ind) tab;
      Printf.fprintf oc "]";
    | Boolnot           -> Printf.fprintf oc "BOOLNOT"
    | Pushtrap ptr      -> Printf.fprintf oc "PUSHTRAP %d" ptr.instr_ind
    | Poptrap           -> Printf.fprintf oc "POPTRAP"
    | Raise             -> Printf.fprintf oc "RAISE"
    | Checksignals      -> Printf.fprintf oc "CHECKSIGNALS"
    | Ccall (n, ind)    -> Printf.fprintf oc "CCALL %d %d" n ind
    | Const n           -> Printf.fprintf oc "CONST %d" n
    | Negint            -> Printf.fprintf oc "NEGINT"
    | Addint            -> Printf.fprintf oc "ADDINT"
    | Subint            -> Printf.fprintf oc "SUBINT"
    | Mulint            -> Printf.fprintf oc "MULINT"
    | Divint            -> Printf.fprintf oc "DIVINT"
    | Modint            -> Printf.fprintf oc "MODINT"
    | Andint            -> Printf.fprintf oc "ANDINT"
    | Orint             -> Printf.fprintf oc "ORINT"
    | Xorint            -> Printf.fprintf oc "XORINT"
    | Lslint            -> Printf.fprintf oc "LSLINT"
    | Lsrint            -> Printf.fprintf oc "LSRINT"
    | Asrint            -> Printf.fprintf oc "ASRINT"
    | Eq                -> Printf.fprintf oc "EQ"
    | Neq               -> Printf.fprintf oc "NEQ"
    | Ltint             -> Printf.fprintf oc "LTINT"
    | Leint             -> Printf.fprintf oc "LEINT"
    | Gtint             -> Printf.fprintf oc "GTINT"
    | Geint             -> Printf.fprintf oc "GEINT"
    | Offsetint ofs     -> Printf.fprintf oc "OFFSETINT %d" ofs
    | Offsetref ofs     -> Printf.fprintf oc "OFFSETREF %d" ofs
    | Isint             -> Printf.fprintf oc "ISINT"
    | Getmethod         -> Printf.fprintf oc "GETMETHOD"
    | Beq (v,ptr)       -> Printf.fprintf oc "BEQ %d %d" v ptr.instr_ind
    | Bneq (v,ptr)      -> Printf.fprintf oc "BNEQ %d %d" v ptr.instr_ind
    | Blint (v,ptr)     -> Printf.fprintf oc "BLINT %d %d" v ptr.instr_ind
    | Bleint (v,ptr)    -> Printf.fprintf oc "BLEINT %d %d" v ptr.instr_ind
    | Bgtint (v,ptr)    -> Printf.fprintf oc "BGTINT %d %d" v ptr.instr_ind
    | Bgeint (v,ptr)    -> Printf.fprintf oc "BGEINT %d %d" v ptr.instr_ind
    | Ultint            -> Printf.fprintf oc "ULTINT"
    | Ugeint            -> Printf.fprintf oc "UGEINT"
    | Bultint (v,ptr)   -> Printf.fprintf oc "BULTINT %d %d" v ptr.instr_ind
    | Bugeint (v,ptr)   -> Printf.fprintf oc "BUGEINT %d %d" v ptr.instr_ind
    | Getpubmet (v,cch) -> Printf.fprintf oc "GETPUBMET %d %d" v cch
    | Getdynmet         -> Printf.fprintf oc "GETDYNMET"
    | Stop              -> Printf.fprintf oc "STOP"
    | Event             -> Printf.fprintf oc "EVENT"
    | Break             -> Printf.fprintf oc "BREAK"
    | Reraise           -> Printf.fprintf oc "RERAISE"
    | Raisenotrace      -> Printf.fprintf oc "RAISENOTRACE"
    | Getstringchar     -> Printf.fprintf oc "GETSTRINGCHAR"
    | Nop               -> Printf.fprintf oc "NOP"
;;
