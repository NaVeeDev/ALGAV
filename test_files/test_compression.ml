open Compression

let blaise_pascal_ () =
  compression "test_files/ressources/Blaise_Pascal/Blaise_Pascal.txt" "test_files/ressources/Blaise_Pascal/test_compression_blaise_pascal.txt.huff";
  ()
;;

let test_compression_ () =
  Printf.printf "\n___ Lancement des tests de compression ___\n";
  try 
    blaise_pascal_ ();
    Printf.printf "___ Tous les tests de compression sont passés ___\n"
  with e -> Printf.printf "_Un test de compression n'est pas passé : \n%s\n" (Printexc.to_string e);
;;