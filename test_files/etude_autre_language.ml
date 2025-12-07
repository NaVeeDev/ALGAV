open Compression 
open Decompression

let analyse () =
  let base_directory = "test_files/ressources/Language/" in
  let files_to_use = [base_directory^"text1"; base_directory^"text2"; base_directory^"text3"; base_directory^"text4"] in 
  
  let rec loop l k =
    match l with 
    | input :: ll -> 
      let compressed_file_name = (base_directory^"compression_language"^(string_of_int k)^".huff") in 
      compression input compressed_file_name false;
      decompression compressed_file_name (base_directory^"decompression_language"^string_of_int k^".txt") false;
      loop ll (k+1)
    | [] -> k
  in
  loop files_to_use 0
;;

let start_language_analyse_ () =
  Printf.printf "\n___ Analyse de l'efficacité de la méthode de Huffman Dynamique sur des fichiers d'informations tels que des .json, .c, etc.. ___\n";
  try 
    let res = analyse () in
    Printf.printf "___ Fin des compressions et decompressions ___\n";
    res
  with e -> Printf.printf "_Un problème est survenu : \n%s\n" (Printexc.to_string e); 0
;;

