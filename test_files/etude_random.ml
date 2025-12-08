open Compression 
open Decompression


let rec generate_texts files =
  let get_random_utf8 () =
    (* renvoie une liste d'entiers (max 4) qui encodent un utf-8 *)
    failwith "todo"
  in

  match files with
  | [] -> ()
  | (file_name, nb_octets) :: ll -> ( 
      let out_channel = open_out file_name in
      (* ajout de BOM *)
      output_byte out_channel 0xEF;
      output_byte out_channel 0xBB;
      output_byte out_channel 0xBF;
      (* génération *)
      for i = 0 to nb_octets do 
        let utf8_list = get_random_utf8 () in 
        List.iter (fun byte -> output_byte out_channel byte) utf8_list  
      done;
  )
;;

let analyse () =
  let base_directory = "test_files/ressources/Random/" in
  let files_to_use = [(base_directory^"randomText1.txt", 700000); (base_directory^"randomText2.txt", 400000)] in

  generate_texts files_to_use;

  let rec loop l k =
    match l with 
    | (input, _) :: ll -> 
      let compressed_file_name = (base_directory^"compression_random"^(string_of_int k)^".huff") in 
      compression input compressed_file_name false;
      decompression compressed_file_name (base_directory^"decompression_random"^string_of_int k^".txt") false;
      loop ll (k+1)
    | [] -> k
  in
  loop files_to_use 0
;;

let start_random_analyse_ () =
  Printf.printf "\n___ Analyse de l'efficacité de la méthode de Huffman Dynamique sur des textes générés aléatoirement ___\n";
  try 
    let res = analyse () in
    Printf.printf "___ Fin des compressions et decompressions ___\n";
    res
  with e -> Printf.printf "_Un problème est survenu : \n%s\n" (Printexc.to_string e); 0
;;

