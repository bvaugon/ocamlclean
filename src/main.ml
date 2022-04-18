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

let source_file = ref None in
let dest_file = ref "a.out" in
let verbose = ref false in
let spec =
  Arg.align [
    ("-o", Arg.Set_string dest_file,
     Printf.sprintf "<outfile> Define output filename (default: %s)"
       !dest_file);
    ("-verbose", Arg.Set verbose, " Verbose mode");
    ("-version", Arg.Unit (fun () -> print_endline Config.version ; exit 0),
     " Print version and exit");
  ] in
let usage = Printf.sprintf "Usage: %s [ OPTIONS ] <file.byte>" Sys.argv.(0) in
let error msg =
  Printf.printf "Error: %s\n" msg;
  Arg.usage spec usage;
  exit 1;
in
let unknow arg =
  if !source_file = None then
    source_file := Some arg
  else
    error (Printf.sprintf "don't know what to do with: `%s'" arg);
in
Arg.parse spec unknow usage;
let print_msg =
  if !verbose then Printf.printf "%s%!" else fun _ -> ()
in
let print_done () = print_msg "done\n" in
let source_file =
  match !source_file with
    | None -> error "please specify a bytecode file.";
    | Some f -> f
in
try
  let pass_counter = ref 0 in
  let rec compress_loop orig_code prim data =
    incr pass_counter;
    print_msg (Printf.sprintf "  * Pass %d... " !pass_counter);
    let code = Step1.clean orig_code data in
    let  ()  = Step2.clean code prim in
    let  ()  = Step3.clean code in
    let data = Data.clean code data in
    let  ()  = Cleanbra.clean code in
    let code = Rmnop.clean code in
    print_done ();
    if orig_code = code then
      (code, data)
    else
      compress_loop code prim data
  in
  print_msg (Printf.sprintf "Loading `%s'... " source_file);
  let bytefile = OByteLib.Bytefile.read source_file in
  let version = bytefile.OByteLib.Bytefile.version in
  let code = OByteLib.Normalised_code.of_code bytefile.OByteLib.Bytefile.code in
  let prim = bytefile.OByteLib.Bytefile.prim in
  let data =
    let data_to_obj = OByteLib.Value.make_to_obj () in
    Array.map data_to_obj bytefile.OByteLib.Bytefile.data in
  let (_, orig_code_length) = OByteLib.Index.find_section bytefile.OByteLib.Bytefile.index OByteLib.Section.CODE in
  let (_, orig_data_length) = OByteLib.Index.find_section bytefile.OByteLib.Bytefile.index OByteLib.Section.DATA in
  let (_, orig_prim_length) = OByteLib.Index.find_section bytefile.OByteLib.Bytefile.index OByteLib.Section.PRIM in
  let orig_file_length =
   let ic = open_in source_file in
   let len = in_channel_length ic in
   close_in ic;
   len in
  let orig_instr_nb = Array.length bytefile.OByteLib.Bytefile.code in
  let orig_data_nb = Array.length bytefile.OByteLib.Bytefile.data in
  let orig_prim_nb = Array.length bytefile.OByteLib.Bytefile.prim in
  print_done ();
  print_msg "Cleaning code:\n";
  let (code, data) = compress_loop code prim data in
  print_msg "Globalising closures... ";
  let (code, data) = Globalise.globalise_closures code data in
  print_done ();
  print_msg "Cleaning environments... ";
  let (code, data) = Cleanenvs.clean_environments code data in
  print_done ();
  print_msg "Cleaning code:\n";
  let (code, data) = compress_loop code prim data in
  print_msg "Cleaning primitives... ";
  let prim = Prim.clean code prim in
  print_done ();
  print_msg (Printf.sprintf "Writting `%s'... " !dest_file);
  let data = Array.map OByteLib.Value.of_obj data in
  let code = OByteLib.Normalised_code.to_code code in
  OByteLib.Bytefile.write !dest_file version
    ?vmpath:bytefile.OByteLib.Bytefile.vmpath
    ?vmarg:bytefile.OByteLib.Bytefile.vmarg
    ~extra:bytefile.OByteLib.Bytefile.extra
    ~dlpt:bytefile.OByteLib.Bytefile.dlpt
    ~dlls:bytefile.OByteLib.Bytefile.dlls
    ~crcs:bytefile.OByteLib.Bytefile.crcs
    ~symb:bytefile.OByteLib.Bytefile.symb
    data prim code;
  let (new_code_length, new_data_length, new_prim_length, new_file_length) =
    let bytefile = OByteLib.Bytefile.read !dest_file in
    let (_, code_length) = OByteLib.Index.find_section bytefile.OByteLib.Bytefile.index OByteLib.Section.CODE in
    let (_, data_length) = OByteLib.Index.find_section bytefile.OByteLib.Bytefile.index OByteLib.Section.DATA in
    let (_, prim_length) = OByteLib.Index.find_section bytefile.OByteLib.Bytefile.index OByteLib.Section.PRIM in
    let ic = open_in !dest_file in
    let file_length = in_channel_length ic in
    close_in ic;
    (code_length, data_length, prim_length, file_length) in
  print_done ();
  let new_instr_nb = Array.length code in
  let new_data_nb = Array.length data in
  let new_prim_nb = Array.length prim in
  let gain o n = if o = n then 1. else (float_of_int o /. float_of_int n) in
  print_msg (
    Printf.sprintf "\n\
Statistics:\n  \
  * Instruction number:  %7d  -> %7d   (/%1.2f)\n  \
  * CODE segment length: %7d  -> %7d   (/%1.2f)\n  \
  * Global data number:  %7d  -> %7d   (/%1.2f)\n  \
  * DATA segment length: %7d  -> %7d   (/%1.2f)\n  \
  * Primitive number:    %7d  -> %7d   (/%1.2f)\n  \
  * PRIM segment length: %7d  -> %7d   (/%1.2f)\n  \
  * File length:         %7d  -> %7d   (/%1.2f)\n\
"
      orig_instr_nb new_instr_nb (gain orig_instr_nb new_instr_nb)
      orig_code_length new_code_length (gain orig_code_length new_code_length)
      orig_data_nb new_data_nb (gain orig_data_nb new_data_nb)
      orig_data_length new_data_length (gain orig_data_length new_data_length)
      orig_prim_nb new_prim_nb (gain orig_prim_nb new_prim_nb)
      orig_prim_length new_prim_length (gain orig_prim_length new_prim_length)
      orig_file_length new_file_length (gain orig_file_length new_file_length)
  );
with
  | Sys_error msg ->
    error msg
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1;
;;
