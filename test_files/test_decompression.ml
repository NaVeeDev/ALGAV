open Decompression

let blaise_pascal_ () =
  decompression "test_files/ressources/Blaise_Pascal/Blaise_Pascal.txt.huff" "test_files/ressources/Blaise_Pascal/test_decompression_blaise_pascal.txt";
  ()
;;

let test_decompression_ () =
  Printf.printf "\n___ Lancement des tests de decompression ___\n";
  try   
    blaise_pascal_ ();
    
    Printf.printf "___ Tous les tests de decompression sont passés ___\n"
  with e -> Printf.printf "_Un test de compression n'est pas passé : \n%s\n" (Printexc.to_string e);
;;