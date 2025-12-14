open Decompression
open Compression

let blaise_pascal_ () =
  let input = "test_files/ressources/Blaise_Pascal/Blaise_Pascal_v2.txt.huff" in 
  let output = "test_files/ressources/Blaise_Pascal/test_decompression_blaise_pascal_v2.txt" in
  decompression input output true;
  
  let check = 
    let command = (* obligé de faire un début de commande avec 'bash' car la commande de base ne se lance pas en bash, donc impossible d'avoir les variables *)
      "bash -c 'cmp -n \"$(stat -c%s test_files/ressources/Blaise_Pascal/Blaise_Pascal_v2.txt)\" \
                test_files/ressources/Blaise_Pascal/Blaise_Pascal_v2.txt \
                <(head -c \"$(stat -c%s test_files/ressources/Blaise_Pascal/Blaise_Pascal_v2.txt)\" " ^
      output ^
      ")'"
in Sys.command command
  in
  if check = 0 then 
    Printf.printf "=> Decompression de Blaise_Pascal.txt_v2.huff réussie !\n"
  else
    Printf.printf "=> Echec de la decompression de Blaise_Pascal_v2.txt.huff ..\n"
;;

let test_decompression_ () =
  Printf.printf "\n___ Lancement des tests de decompression ___\n";
  try   
    blaise_pascal_ ();
    
    Printf.printf "___ Tous les tests de decompression sont passés ___\n"
  with e -> Printf.printf "_Un test de compression n'est pas passé : \n%s\n" (Printexc.to_string e);
;;