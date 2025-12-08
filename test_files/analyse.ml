let analyse_ () =  
  (* reset des stats *)
  let channel = open_out_gen [Open_trunc] 0o644 "compression.txt" in
  close_out channel;
  let channel = open_out_gen [Open_trunc] 0o644 "decompression.txt" in
  close_out channel;
  
  (* lancement des jeux d'étude *)
  let n_gutenberg = Etude_gutemberg.start_gutemberg_analyse_ () in 
  let n_random = Etude_random.start_random_analyse_ () in 
  let n_language = Etude_autre_language.start_language_analyse_ () in 

  let infos_channel_compression = open_in "compression.txt" in 
  let infos_channel_decompression = open_in "decompression.txt" in 
  
  let get_next_row_infos channel = 
    try 
      let line = input_line channel in 
      let line_splited = String.split_on_char ';' line in
      let len = List.length line_splited in
      Some (List.nth line_splited 0, float_of_string (List.nth line_splited 2), float_of_string (List.nth line_splited (len - 2)), float_of_string (List.nth line_splited (len - 1)))
    with 
    | End_of_file -> None 
    | e -> failwith ("issue in next row : " ^ Printexc.to_string e)
  in

  (* analyse des résultats *)
  Printf.printf "\n\n\n=> TEXTES DE GUTEMBERG <=\n";
  for i = 0 to (n_gutenberg-1) do 
    let comp_opt = get_next_row_infos infos_channel_compression in 
    let decompr_opt = get_next_row_infos infos_channel_decompression in 
    match comp_opt, decompr_opt with 
    | Some (name, nb_octets_entree_compr, taux_compression, temps_de_compression), Some (_, nb_octets_entree_decompr, taux_decompression, temps_de_decompression) ->
      (
        Printf.printf "%s :\n    Taux de compression   : %f\n    Taux de décompression : %f\n    Octets écrits chaque milliseconde : \n       -compression   = %f\n       -décompression = %f\n\n"
        name taux_compression taux_decompression (nb_octets_entree_compr /. temps_de_compression) (nb_octets_entree_decompr /. temps_de_decompression)
      )
    | _ -> failwith "issue with n_gutemberg" 
  done;

  Printf.printf "\n\n\n=> TEXTES GÉNÉRÉS ALÉATOIREMENT <=\n";
  for i = 0 to (n_random-1) do 
    let comp_opt = get_next_row_infos infos_channel_compression in 
    let decompr_opt = get_next_row_infos infos_channel_decompression in 
    match comp_opt, decompr_opt with 
    | Some (name, nb_octets_entree_compr, taux_compression, temps_de_compression), Some (_, nb_octets_entree_decompr, taux_decompression, temps_de_decompression) ->
      (
        Printf.printf "%s :\n    Taux de compression   : %f\n    Taux de décompression : %f\n    Octets écrits chaque milliseconde : \n       -compression   = %f\n       -décompression = %f\n\n"
        name taux_compression taux_decompression (nb_octets_entree_compr /. temps_de_compression) (nb_octets_entree_decompr /. temps_de_decompression)
      )
    | _ -> failwith "issue with n_random" 
  done;

  Printf.printf "\n\n\n=> TEXTES AVEC DES EXTENSIONS DE LANGUAGES DE PROGRAMMATION <=\n";
  for i = 0 to (n_language-1) do 
    let comp_opt = get_next_row_infos infos_channel_compression in 
    let decompr_opt = get_next_row_infos infos_channel_decompression in 
    match comp_opt, decompr_opt with 
    | Some (name, nb_octets_entree_compr, taux_compression, temps_de_compression), Some (_, nb_octets_entree_decompr, taux_decompression, temps_de_decompression) ->
      (
        Printf.printf "%s :\n    Taux de compression   : %f\n    Taux de décompression : %f\n    Octets écrits chaque milliseconde : \n       -compression   = %f\n       -décompression = %f\n\n"
        name taux_compression taux_decompression (nb_octets_entree_compr /. temps_de_compression) (nb_octets_entree_decompr /. temps_de_decompression)
      )
    | _ -> failwith "issue with n_language" 
  done;
  
;;

analyse_ ()