let aux_analyseur

let analyse_ () =
  (* reset des stats *)
  let channel = open_out_gen [Open_trunc] 0o644 "compression.txt" in
  close_out channel;
  
  (* lancement des jeux d'étude *)
  let n_gutenberg = Etude_gutemberg.start_gutemberg_analyse_ () in 
  let n_random = Etude_random.start_random_analyse_ () in 
  let n_language = Etude_autre_language.start_language_analyse_ () in 

  (* analyse des résultats *)

  ()
  
;;

analyse_ ()