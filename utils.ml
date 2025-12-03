let nth_bit (n : int) (byte : int) : int =
  if n < 0 || n >= 8 then 
    invalid_arg ("n devrait être une valeur comprise dans [0-7]")
  else if byte < 0 || byte > 255 then
    invalid_arg ("byte devrait être une valeur comprise dans [0-255]")
  else 
    (byte lsr (7 - n)) land 1
;; 

let byte_of_bits (bits : int list) : int =
  let rec aux bits acc =
    match bits with
    | [] -> acc
    | b :: ll -> aux ll (acc * 2 + b)
  in
  aux bits 0
;;


let lecture (bin_file : string) : unit =
  try
    let channel = Stdlib.open_in_bin bin_file in

    let rec loop () =
      try
        (* lit 8 bits, écrit sous forme d'un entier *)
        let byte = input_byte channel in
        (* affiche chacun des bits *)
        for i=0 to 7 do 
          Printf.printf "%d " (nth_bit i byte)
        done;
        loop ()
      with End_of_file -> ()
    in
    loop ();

    close_in channel
  with Sys_error _ -> invalid_arg ("File not found: " ^ bin_file)
;;


let ecriture (input_file : string) (output_file : string) : unit =
  let byte_to_int l = List.fold_right(fun x a -> 2*a+x) l 0 in
  try 
    let ochannel = open_out_bin output_file in
    let ichannel = open_in input_file in

    let rec loop l =
      try 
        let char = input_char ichannel in
        let new_l = (match char with
        | '\n' -> raise End_of_file
        | '1' -> 1 :: l  
        | '0' -> 0 :: l
        | _ -> invalid_arg ("The input file is not properly formatted.")) in
        let new_l = if pos_in ichannel mod 8 = 0 then (
          output_byte ochannel (byte_to_int new_l);
          []
        ) else new_l in 
        loop new_l;
        with | Invalid_argument _ -> invalid_arg ("The input file is not properly formatted.")
        | End_of_file -> if l <> [] then output_byte ochannel (byte_to_int l); ()
      in loop [];
      close_in ichannel;
      close_out ochannel;
    with Sys_error _ -> invalid_arg "File Not Found"
;;
let compare_files (f1 : string) (f2 : string) : bool =
  failwith "todo";