let test_nth_bit_ () =
  assert (0 == (Utils.nth_bit 0 0));
  (* 57 s'écrit '00111001' en binaire *)
  assert (0 == (Utils.nth_bit 0 57));
  assert (0 == (Utils.nth_bit 1 57));
  assert (1 == (Utils.nth_bit 2 57));
  assert (1 == (Utils.nth_bit 3 57));
  assert (1 == (Utils.nth_bit 4 57));
  assert (0 == (Utils.nth_bit 5 57));
  assert (0 == (Utils.nth_bit 6 57));
  assert (1 == (Utils.nth_bit 7 57));
  (* 40 s'écrit '00101000' en binaire *)
  assert (0 == (Utils.nth_bit 0 40));
  assert (0 == (Utils.nth_bit 1 40));
  assert (1 == (Utils.nth_bit 2 40));
  assert (0 == (Utils.nth_bit 3 40));
  assert (1 == (Utils.nth_bit 4 40));
  assert (0 == (Utils.nth_bit 5 40));
  assert (0 == (Utils.nth_bit 6 40));
  assert (0 == (Utils.nth_bit 7 40));
  (* 132 s'écrit '10000100' en binaire *)
  assert (1 == (Utils.nth_bit 0 132));
  assert (0 == (Utils.nth_bit 1 132));
  assert (0 == (Utils.nth_bit 2 132));
  assert (0 == (Utils.nth_bit 3 132));
  assert (0 == (Utils.nth_bit 4 132));
  assert (1 == (Utils.nth_bit 5 132));
  assert (0 == (Utils.nth_bit 6 132));
  assert (0 == (Utils.nth_bit 7 132));
  (* check des valeurs en dehors des limites *)
  try 
    let _ = Utils.nth_bit 0 (-1) in
    assert false
  with Invalid_argument _ -> 
    assert true;
  try 
    let _ = Utils.nth_bit 6 (256) in
    assert false
  with Invalid_argument _ -> 
    assert true;
  try 
    let _ = Utils.nth_bit (-1) 189 in
    assert false
  with Invalid_argument _ -> 
    assert true;
  try 
    let _ = Utils.nth_bit 8 247 in
    assert false
  with Invalid_argument _ -> 
    assert true;
;;

(* Crée un fichier [file_name] de type binaire, avec pour éléments les entiers de la liste [list] *)
let create_binary_file (file_name : string) (list : int list) : unit =
  let channel = open_out_bin file_name in
  let rec loop l =
    match l with 
    | [] -> close_out channel
    | i :: ll -> output_byte channel i; loop ll 
  in
  loop list

let test_file = "test_files/ressources"


let test_lecture_ () =
  
  let content = [] in 
  let file= test_file^"/test_lecture_empty.bin" in 
  create_binary_file file content;
  Printf.printf "test_lecture_empty :\n";
  Utils.lecture file;
  print_newline ();

  let content = [0] in 
  let file= test_file^"/test_lecture_0.bin" in 
  create_binary_file file content;
  Printf.printf "test_lecture_0 :\n   0 0 0 0 0 0 0 0\n   ";
  Utils.lecture file;
  print_newline ();

  let content = [1] in 
  let file= test_file^"/test_lecture_1.bin" in 
  create_binary_file file content;
  Printf.printf "test_lecture_1 :\n   0 0 0 0 0 0 0 1\n   ";
  Utils.lecture file;
  print_newline ();

  let content = [130] in 
  let file = test_file^"/test_lecture_2.bin" in 
  create_binary_file file content;
  Printf.printf "test_lecture_2 :\n   1 0 0 0 0 0 1 0\n   ";
  Utils.lecture file;
  print_newline ();

  let content = [130; 1] in 
  let file = test_file^"/test_lecture_3.bin" in 
  create_binary_file file content;
  Printf.printf "test_lecture_3 :\n   1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1\n   ";
  Utils.lecture file;
  print_newline ();

  let content = [9; 42; 243; 87; 0; 32] in 
  let file = test_file^"/test_lecture_4.bin" in 
  create_binary_file file content;
  Printf.printf "test_lecture_4 :\n   0 0 0 0 1 0 0 1 0 0 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 0 1 0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0\n   ";
  Utils.lecture file;
  print_newline ()



let test_ecriture_ () = 
  let file = test_file^"/test_ecriture_empty.txt" in
  let output = test_file^"/test_ecriture_empty.bin" in 
  Printf.printf "test_ecriture_empty :\n   ";
  Utils.ecriture file output;
  Utils.lecture output;
  print_newline ();

  let file = test_file^"/test_ecriture_0.txt" in
  let output = test_file^"/test_ecriture_0.bin" in 
  Printf.printf "test_ecriture_0 :\n   0\n   ";
  Utils.ecriture file output;
  Utils.lecture output;
  print_newline ();

  let file = test_file^"/test_ecriture_1.txt" in
  let output = test_file^"/test_ecriture_1.bin" in 
  Printf.printf "test_ecriture_1 :\n   0 1\n   ";
  Utils.ecriture file output;
  Utils.lecture output;
  print_newline ();

  let file = test_file^"/test_ecriture_2.txt" in
  let output = test_file^"/test_ecriture_2.bin" in 
  Printf.printf "test_ecriture_2 :\n   0 0 0 1 1 1 1\n   ";
  Utils.ecriture file output;
  Utils.lecture output;
  print_newline ();

  let file = test_file^"/test_ecriture_3.txt" in
  let output = test_file^"/test_ecriture_3.bin" in 
  Printf.printf "test_ecriture_3 :\n   1 1 1 1 1 1 1 1\n   ";
  Utils.ecriture file output;
  Utils.lecture output;
  print_newline ();

  let file = test_file^"/test_ecriture_4.txt" in
  let output = test_file^"/test_ecriture_4.bin" in 
  Printf.printf "test_ecriture_4 :\n   1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0\n   ";
  Utils.ecriture file output;
  Utils.lecture output;
  print_newline ();

  let file = test_file^"/test_ecriture_5.txt" in
  let output = test_file^"/test_ecriture_5.bin" in 
  Printf.printf "test_ecriture_5 :\n   0 1 0 1 0 1 1 0 1 1 0 1 0 1 0\n   ";
  Utils.ecriture file output;
  Utils.lecture output;
  print_newline ();
;;

(* Appeler cette fonction pour executer les tests *)
let test_utils_ () =
  Printf.printf "\n___ Lancement des tests de test_utils.ml ___\n";
  try 
    test_nth_bit_  ();
    test_lecture_  ();
    test_ecriture_ ();
    Printf.printf "___ Tous les tests de test_utils.ml sont passés ___\n"
  with e -> Printf.printf "_Un test de test_utils.ml n'est pas passé : \n%s\n" (Printexc.to_string e);
;;