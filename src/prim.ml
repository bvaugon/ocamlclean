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
  let (offset, length) =
    try
      Index.find_section index Index.Prim
    with Not_found ->
      raise (Exn "prim section not found")
  in
  seek_in ic offset;
  let buf = Buffer.create 16 in
  let rec f i res =
    if i <> length then
      let c = input_char ic in
      if int_of_char c <> 0 then
        begin
          Buffer.add_char buf c;
          f (i + 1) res
        end
      else
        let name = Buffer.contents buf in
        Buffer.clear buf;
        f (i + 1) (name :: res)
    else if Buffer.length buf <> 0 then
      raise (Exn "unexpected end of prim section")
    else
      res
  in
  Array.of_list (List.rev (f 0 []))
;;

let clean code orig_prim =
  let nb_instr = Array.length code in
  let nb_prim = Array.length orig_prim in
  let map = Array.make nb_prim None in
  let invmap = Array.make nb_prim 0 in
  let counter = ref 0 in
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
  for i = 0 to nb_instr - 1 do
    match code.(i) with
      | Instr.Ccall (n, p) -> code.(i) <- Instr.Ccall (n, remap p);
      | _ -> ()
  done;
  let new_prim = Array.init !counter (fun p -> orig_prim.(invmap.(p))) in
  new_prim
;;

let export oc prim =
  Array.iter (Printf.fprintf oc "%s\000") prim;
;;

let no_side_effect prim_name =
  match prim_name with
    | "caml_alloc_dummy"
    | "caml_alloc_dummy_float"
    | "caml_array_unsafe_get_float"
    | "caml_array_unsafe_get"
    | "caml_float_of_string"
    | "caml_int_of_float"
    | "caml_float_of_int"
    | "caml_neg_float"
    | "caml_abs_float"
    | "caml_add_float"
    | "caml_sub_float"
    | "caml_mul_float"
    | "caml_div_float"
    | "caml_exp_float"
    | "caml_floor_float"
    | "caml_fmod_float"
    | "caml_frexp_float"
    | "caml_ldexp_float"
    | "caml_log_float"
    | "caml_log10_float"
    | "caml_modf_float"
    | "caml_sqrt_float"
    | "caml_power_float"
    | "caml_sin_float"
    | "caml_sinh_float"
    | "caml_cos_float"
    | "caml_cosh_float"
    | "caml_tan_float"
    | "caml_tanh_float"
    | "caml_asin_float"
    | "caml_acos_float"
    | "caml_atan_float"
    | "caml_atan2_float"
    | "caml_ceil_float"
    | "caml_eq_float"
    | "caml_neq_float"
    | "caml_le_float"
    | "caml_lt_float"
    | "caml_ge_float"
    | "caml_gt_float"
    | "caml_float_compare"
    | "caml_classify_float"
    | "caml_gc_stat"
    | "caml_gc_quick_stat"
    | "caml_gc_counters"
    | "caml_gc_get"
    | "caml_int_compare"
    | "caml_int32_neg"
    | "caml_int32_add"
    | "caml_int32_sub"
    | "caml_int32_mul"
    | "caml_int32_and"
    | "caml_int32_or"
    | "caml_int32_xor"
    | "caml_int32_shift_left"
    | "caml_int32_shift_right"
    | "caml_int32_shift_right_unsigned"
    | "caml_int32_of_int"
    | "caml_int32_to_int"
    | "caml_int32_of_float"
    | "caml_int32_to_float"
    | "caml_int32_compare"
    | "caml_int32_bits_of_float"
    | "caml_int32_float_of_bits"
    | "caml_int64_neg"
    | "caml_int64_add"
    | "caml_int64_sub"
    | "caml_int64_mul"
    | "caml_int64_and"
    | "caml_int64_or"
    | "caml_int64_xor"
    | "caml_int64_shift_left"
    | "caml_int64_shift_right"
    | "caml_int64_shift_right_unsigned"
    | "caml_int64_of_int"
    | "caml_int64_to_int"
    | "caml_int64_of_float"
    | "caml_int64_to_float"
    | "caml_int64_of_int32"
    | "caml_int64_to_int32"
    | "caml_int64_of_nativeint"
    | "caml_int64_to_nativeint"
    | "caml_int64_compare"
    | "caml_int64_bits_of_float"
    | "caml_int64_float_of_bits"
    | "caml_nativeint_neg"
    | "caml_nativeint_add"
    | "caml_nativeint_sub"
    | "caml_nativeint_mul"
    | "caml_nativeint_and"
    | "caml_nativeint_or"
    | "caml_nativeint_xor"
    | "caml_nativeint_shift_left"
    | "caml_nativeint_shift_right"
    | "caml_nativeint_shift_right_unsigned"
    | "caml_nativeint_of_int"
    | "caml_nativeint_to_int"
    | "caml_nativeint_of_float"
    | "caml_nativeint_to_float"
    | "caml_nativeint_of_int32"
    | "caml_nativeint_to_int32"
    | "caml_nativeint_compare"
    | "caml_static_alloc"
    | "caml_obj_is_block"
    | "caml_obj_tag"
    | "caml_obj_block"
    | "caml_obj_dup"
    | "caml_lazy_make_forward"
    | "caml_get_public_method"
    | "caml_ml_string_length"
    | "caml_string_equal"
    | "caml_string_notequal"
    | "caml_string_compare"
    | "caml_string_lessthan"
    | "caml_string_lessequal"
    | "caml_string_greaterthan"
    | "caml_string_greaterequal"
    | "caml_is_printable" -> true
    | _ -> false
;;
