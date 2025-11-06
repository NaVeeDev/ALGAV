open Compression

let blaise_pascal_ () =
  compression "ressources/Blaise_Pascal/Blaise_Pascal.txt";
  failwith "TODO : must compare new created file with the one the prof gave us";
;;

let test_compression_ () =
  Printf.printf "\n___ Lancement des tests de compression ___\n";
  try 
    blaise_pascal_ ();
    Printf.printf "___ Tous les tests de compression sont passés ___\n"
  with e -> Printf.printf "_Un test de compression n'est pas passé : \n%s\n" (Printexc.to_string e);
;;