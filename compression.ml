open Primitive

let compression file =
    let H_ = {content = Leaf (EmptyChar, 0); parent = None} in
    let table = CharaMap.empty in
    let table = CharaMap.add EmptyChar H_ table in
    try 
    let channel = Stdlib.open_in file in
        let rec loop acc_code acc_table =
            try 
                let s = input_char channel in
                let new_code =  if mem s acc_table then 
                    code (Char s) acc_table
                    else 
                        try 
                            (code (EmptyChar) acc_table)@(initial_code s)
                        with Not_found -> "Read a character whose code was not given in the alphabet"
                    in
                loop new_code (modification H_ acc_table s)
            with End_of_file -> close_in channel; acc_code
        in
        loop [] table
    with  Sys_error _ -> invalid_arg ("File not found: " ^ file)