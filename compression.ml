open Primitive

let compression input_file output_file=
    let _H = {content = Leaf (EmptyChar, 0); parent = None} in
    let table = CharaMap.empty in
    let table = CharaMap.add EmptyChar _H table in
    try 
    let in_channel = open_in input_file in
    let out_channel = open_out output_file in
        let rec loop acc_table =
            try 
                let s = input_char in_channel in
                let new_code =  if mem (Char s) acc_table then 
                    code (Char s) acc_table
                    else 
                            (code (EmptyChar) acc_table)@(initial_code s)
                    in
                List.iter (fun bit -> output_string out_channel (string_of_int bit)) new_code;
                loop (modification _H acc_table s)
            with End_of_file -> close_in in_channel; close_out out_channel;
        in
        loop table;
    with  Sys_error _ -> raise (Invalid_argument ("File not found: " ^ input_file ^ " or " ^ output_file))