open Decompression
open Compression

let blaise_pascal_ () =
  let input = "test_files/ressources/Blaise_Pascal/Blaise_Pascal.txt.huff" in 
  let output = "test_files/ressources/Blaise_Pascal/test_decompression_blaise_pascal.txt" in
  decompression input output true;
  
  (* compression "test_files/ressources/Blaise_Pascal/Blaise_Pascal.txt" "test_files/ressources/Blaise_Pascal/inter.huff";
  decompression "test_files/ressources/Blaise_Pascal/inter.huff" "test_files/ressources/Blaise_Pascal/res.txt"; *)

;;

let test_decompression_ () =
  Printf.printf "\n___ Lancement des tests de decompression ___\n";
  try   
    blaise_pascal_ ();
    
    Printf.printf "___ Tous les tests de decompression sont passés ___\n"
  with e -> Printf.printf "_Un test de compression n'est pas passé : \n%s\n" (Printexc.to_string e);
;;