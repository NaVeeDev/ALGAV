open Compression 
open Decompression

let analyse () =
  let base_directory = "test_files/ressources/Random/" in
  let files_to_use = [(base_directory^"randomText1.txt");(base_directory^"randomText2.txt");(base_directory^"all_a.txt");(base_directory^"randomText3.txt");(base_directory^"worst_case.txt");(base_directory^"all_a_b.txt");(base_directory^"randomText4.txt")] in

  (* génération d'un fichier sur le terminal avec : head -c 3000 /dev/urandom | base64 | head -c sizeX > random_text_sizeX.txt
      Puis traduction en UTF-8 *)

  let rec loop l k =
    match l with 
    | input :: ll -> 
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

