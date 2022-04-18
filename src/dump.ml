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

let buf_len = Sys.max_array_length

let import ic length =
  let mem_size = length / buf_len + 1 in
  let mem = Array.make mem_size [||] in
  for i = 0 to mem_size - 2 do mem.(i) <- Array.make buf_len (-1) done;
  mem.(mem_size - 1) <- Array.make (length - buf_len * (mem_size - 1)) (-1);
  let rec fill i j cpt =
    if cpt <> 0 then
      if j = buf_len then fill (succ i) 0 cpt
      else (
        mem.(i).(j) <- input_byte ic;
        fill i (succ j) (pred cpt);
      )
  in
  fill 0 0 length;
  mem
;;

let export oc = Array.iter (Array.iter (output_byte oc));;

let size mem = Array.fold_left (fun acc tbl -> acc + Array.length tbl) 0 mem;;

let parse ic index section =
  try
    let (offset, length) = OByteLib.Index.find_section index section in
    seek_in ic offset;
    (section, import ic length)
  with Not_found -> (section, [||])
;;

let parse_beginning ic index =
  let rec compute_min min rest =
    match rest with
      | [] -> min
      | (_, offset, _) :: tl ->
        if offset < min then compute_min offset tl else compute_min min tl
  in
  let size = compute_min (in_channel_length ic) index in
  seek_in ic 0;
  import ic size
;;
