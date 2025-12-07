open Compression

let blaise_pascal_ () =
  let input = "test_files/ressources/Blaise_Pascal/Blaise_Pascal.txt" in 
  let output = "test_files/ressources/Blaise_Pascal/test_compression_blaise_pascal.txt.huff" in
  compression input output true;

  let check = 
    Sys.command ("cmp "^output^" test_files/ressources/Blaise_Pascal/Blaise_Pascal.txt.huff")
  in
  if check = 0 then 
    Printf.printf "=> Compression de Blaise_Pascal.txt réussie !\n"
  else
    Printf.printf "=> Echec de la compression de Blaise_Pascal.txt ..\n"
;;

let test_compression_ () =
  Printf.printf "\n___ Lancement des tests de compression ___\n";
  try 
    blaise_pascal_ ();
        
    Printf.printf "___ Tous les tests de compression sont passés ___\n"
  with e -> Printf.printf "_Un test de compression n'est pas passé : \n%s\n" (Printexc.to_string e);
;;

