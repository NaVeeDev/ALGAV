let nth_bit (n : int) (byte : int) : int =
  if n < 0 || n >= 8 then 
    invalid_arg ("n devrait être une valeur comprise dans [0-7]")
  else if byte < 0 || byte > 255 then
    invalid_arg ("byte devrait être une valeur comprise dans [0-255]")
  else 
    (byte lsr (7 - n)) land 1
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
  let rec write_byte oc l =
    match l with 
    | [] -> ()
    | x :: ll -> output_byte oc x; write_byte oc ll
  in
  try 
    let ochannel = open_out_bin output_file in
    let ichannel = open_in input_file in

    let rec loop l =
      try 
        let char = input_char ichannel in
        
        let new_l = if pos_in ichannel mod 8 = 0 then (
          write_byte ochannel l;
          []
        ) else l in
        match char with
        | '\n' -> ()
        | '1' -> loop (1 :: new_l)  
        | '0' -> loop (0 :: new_l)
        | _ -> invalid_arg ("The input file is not properly formatted.") 
        
        with | Invalid_argument _ -> invalid_arg ("The input file is not properly formatted.")
        | End_of_file -> ()
      in loop [];
      close_in ichannel;
      close_out ochannel;
    with Sys_error _ -> invalid_arg "File Not Found"
;;