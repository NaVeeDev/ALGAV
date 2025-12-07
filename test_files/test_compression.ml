open Compression

let blaise_pascal_ () =
  let input = "test_files/ressources/Blaise_Pascal/Blaise_Pascal.txt" in 
  let output = "test_files/ressources/Blaise_Pascal/test_compression_blaise_pascal.txt.huff" in
  compression input output true;
;;

let test_compression_ () =
  Printf.printf "\n___ Lancement des tests de compression ___\n";
  try 
    blaise_pascal_ ();
        
    Printf.printf "___ Tous les tests de compression sont passés ___\n"
  with e -> Printf.printf "_Un test de compression n'est pas passé : \n%s\n" (Printexc.to_string e);
;;

