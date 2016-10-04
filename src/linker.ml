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

let export filename code prim data begn dumps =
  let oc =
    open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] 0o751 filename
  in
  Dump.export oc begn;
  let code_offset = pos_out oc in
  Code.export oc code;
  let prim_offset = pos_out oc in
  let code_length = prim_offset - code_offset in
  Prim.export oc prim;
  let data_offset = pos_out oc in
  let prim_length = data_offset - prim_offset in
  Data.export oc data;
  let end_offset = pos_out oc in
  let data_length = end_offset - data_offset in
  let rec export_dumps dumps_rest dumps_index =
    match dumps_rest with
      | [] -> List.rev dumps_index
      | (section, mem) :: tl ->
        let length = Dump.size mem in
        if length <> 0 then
          let offset = pos_out oc in
          Dump.export oc mem;
          export_dumps tl ((section, offset, length) :: dumps_index)
        else
          export_dumps tl dumps_index
  in
  Index.export oc (
    (Index.Code, code_offset, code_length) ::
      (Index.Prim, prim_offset, prim_length) ::
      (Index.Data, data_offset, data_length) ::
      (export_dumps dumps [])
  );
  let file_length = pos_out oc in
  close_out oc;
  (code_length, data_length, prim_length, file_length)
;;
