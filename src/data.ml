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

let parse ic index =
  let (offset, _) =
    try
      Index.find_section index Index.Data
    with Not_found ->
      raise (Exn "code section not found")
  in
  seek_in ic offset;
  let (data : Obj.t array) = input_value ic in
  data
;;

let clean code orig_data =
  let nb_data = Array.length orig_data in
  let nb_instr = Array.length code in
  let map = Array.make nb_data None in
  let invmap = Array.make nb_data 0 in
  let counter = ref 12 in
  let remap p =
    match map.(p) with
      | None ->
        let new_p = !counter in
        counter := succ new_p;
        map.(p) <- Some new_p;
        invmap.(new_p) <- p;
        new_p
      | Some new_p -> new_p
  in
  for i = 0 to !counter - 1 do
    map.(i) <- Some i;
    invmap.(i) <- i;
  done;
  for i = 0 to nb_instr - 1 do
    match code.(i) with
      | Instr.Getglobal p ->
        code.(i) <- Instr.Getglobal (remap p);
      | Instr.Getglobalfield (p, n) ->
        code.(i) <- Instr.Getglobalfield (remap p, n);
      | _ -> ()
  done;
  for i = 0 to nb_instr - 1 do
    match code.(i) with
      | Instr.Setglobal p ->
        begin match map.(p) with
          | None -> code.(i) <- Instr.Nop;
          | Some new_p -> code.(i) <- Instr.Setglobal new_p;
        end
      | _ -> ()
  done;
  let new_data = Array.init !counter (fun p -> orig_data.(invmap.(p))) in
  new_data
;;

let export oc data =
  output_value oc data;
;;
