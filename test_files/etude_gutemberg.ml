open Compression 
open Decompression

let analyse () =
  let base_directory = "test_files/ressources/Gutemberg/" in
  let files_to_use = [base_directory^"Curiosités judiciaires et historiques du moyen âge. Procès contre les animaux.txt";
                      base_directory^"Dernières lettres d'un bon jeune homme à sa cousine Madeleine.txt";
                      base_directory^"Histoire des salons de Paris (Tome 3 sur 6).txt"; 
                      base_directory^"La Mère de la Marquise.txt";
                      base_directory^"Pierre de Villerglé.txt"]
  in 
  
  let rec loop l k =
    match l with 
    | input :: ll -> 
      let compressed_file_name = (base_directory^"compression_livre"^(string_of_int k)^".huff") in 
      compression input compressed_file_name false;
      decompression compressed_file_name (base_directory^"decompression_livre"^string_of_int k^".txt") false;
      loop ll (k+1)
    | [] -> k
  in
  loop files_to_use 0

;;

let start_gutemberg_analyse_ () =
  Printf.printf "\n___ Analyse de l'efficacité de la méthode de Huffman Dynamique sur des textes de gutemberg ___\n";
  try 
    let res = analyse () in
    Printf.printf "___ Fin des compressions et decompressions ___\n";
    res
  with e -> Printf.printf "_Un problème est survenu : \n%s\n" (Printexc.to_string e); 0
;;

