open Primitive

let compression input_file output_file =
    let _H = {content = Leaf (EmptyChar, 0); parent = None} in
    let table = CharaMap.empty in
    let table = CharaMap.add EmptyChar _H table in
    try 
        let in_channel = open_in input_file in
        let out_channel = open_out output_file in

        let rec split_at_i i l =
            if i <= 0 then 
                ([], l)
            else
                match l with
                | [] -> ([], [])
                | e :: ll ->
                    let left, right = split_at_i (i-1) ll in
                    (e :: left, right)
        in

        let byte_counter = ref 3 in 

        let rec write_in_output_file buffer =
            if List.length buffer >= 8 then
                let byte_bits, rest = split_at_i 8 buffer in
                let byte_value = List.fold_left (fun acc b -> acc * 2 + b) 0 (byte_bits) in
                output_byte out_channel byte_value;
                byte_counter:= !byte_counter +1;
                write_in_output_file rest
            else buffer
        in

        output_byte out_channel 0xEF;
        output_byte out_channel 0xBB;
        output_byte out_channel 0xBF;

        let bom = Uchar.of_int 0xFEFF in 
        let table = insert table bom in

        let utf8_decoder (ic : in_channel) =
            let decoder = Uutf.decoder ~encoding:`UTF_8 (`Channel ic) in
            fun () ->
                match Uutf.decode decoder with
                | `Uchar u -> Some u
                | `End -> None
                | `Malformed _ -> None
                | _ -> None
        in

        let get_next_char = utf8_decoder in_channel in 

        let rec loop acc_table buffer =

        match get_next_char () with
            | Some s when Uchar.to_int s = 0xFEFF ->
                loop acc_table buffer
            | Some s when Uchar.to_int s = 0x0D ->
                loop acc_table buffer
            | Some s ->
                (* read a character from input file *)
                (* getting the encodage of that character *)
                let encodage = 
                    if mem (Char s) acc_table then (
                        (* 
                            -> character already encoutered : 
                           value is the character's code in the table
                        *)
                        let codage = code (Char s) acc_table in 
                        codage
                    ) else ( 
                        (* 
                            -> first time we see the character : 
                           value is (code of EMPTY character in the table) ^ (UTF-8 code of the character)
                        *)
                        let code_EMPTY = code (EmptyChar) acc_table in 
                        let code_UTF8 = initial_code s in 
                        let code_complet = code_EMPTY @ code_UTF8 in 
                        code_complet
                        
                    )
                in
                (* writing phase in output file *)
                    (* add to buffer *)
                    let buffer = buffer @ encodage in 
                    
                    (* if at least 8 bits, write byte in output file *)
                    let buffer = write_in_output_file buffer in 

                (* updating acc_table *)
                let acc_table = modification _H acc_table s in

                (* next UTF-8 to encode *)
                loop acc_table buffer
            | None -> 
                (* rempli le dernier octet avec le buffer restant, ou juste un octet vide *)
                let fill_last_buffer_with_0 k_left buffer =
                    buffer @ List.init k_left (fun _ -> 0)
                in
                let len = List.length buffer in
                if (len > 0) then 
                    (let last_buffer = fill_last_buffer_with_0 (8-(len mod 8)) buffer in
                    ignore (write_in_output_file last_buffer))
                ;
                close_in in_channel; 
                close_out out_channel;
        in
        loop table [];
    with  
        Sys_error _ -> 
            raise (Invalid_argument ("File not found: " ^ input_file ^ " or " ^ output_file))
;;
