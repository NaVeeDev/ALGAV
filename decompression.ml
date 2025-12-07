open Primitive
open Utils

let decompression input_file output_file visual =
  let _H = {content = Leaf (EmptyChar, 0); parent = None} in
  let table = CharaMap.empty in
  let table = CharaMap.add EmptyChar _H table in

  try
    (
    let in_channel = open_in input_file in
    let out_channel = open_out output_file in
    (* *)
    let get_next_byte () = 
      try
        let byte = input_byte in_channel in
        let rec aux i =
          if i > 7 then 
            []
          else
            nth_bit i byte :: aux (i+1)
        in
        let res = aux 0 in
        Some res
      with End_of_file -> None
    in
    (* *)
    let hashtag_is_next hashtag_code input_buffer =
      let rec aux hashtag_left buffer_left input_buffer =
        match hashtag_left with 
        | e :: ll -> (
          match buffer_left with 
          | b :: bb ->
            if b = e then 
              aux ll bb input_buffer
            else
              (false, input_buffer)
          | [] -> 
            (
              (* not enough bits read in the input_buffer *)
              let next = get_next_byte () in 
              match next with 
              | Some new_buffer ->
                (
                  match new_buffer with 
                  | ee :: llll ->
                    (
                      if ee = e then
                        aux ll llll (input_buffer @ new_buffer)
                      else
                        (false, input_buffer @ new_buffer)
                    )
                  | [] -> (false, input_buffer)
                )
              | None -> (false, input_buffer)
            )
        )
        | [] -> (
          (true, buffer_left)
        )
      in
      aux hashtag_code input_buffer input_buffer
    in
    (* *)
    let read_next_utf8 input_buffer =
      let rec n_bytes buffer =
        if List.length buffer >= 5 then 
          match buffer with 
          | b0 :: b1 :: b2 :: b3 :: b4 :: ll -> 
            if b0 = 0 then (1, buffer)
            else if b1 = 1 && b2 = 0 then (2, buffer)
            else if b1 = 1 && b2 = 1 && b3 = 0 then (3, buffer)
            else if b1 = 1 && b2 = 1 && b3 = 1 && b4 = 0 then (4, buffer)
            else failwith "utf8 not recognized"
          | _ -> failwith "not happening - protected"
        else
          (
            let next = get_next_byte () in 
              match next with 
              | Some new_buffer -> n_bytes (buffer @ new_buffer)
              | None -> failwith "utf8 wrongly formatted"

          )
      in
      let rec first_8_bits_from_bits buffer =
        if List.length buffer >= 8 then 
          (
            match buffer with 
            | b0::b1::b2::b3::b4::b5::b6::b7::ll ->
              ([b0; b1; b2; b3; b4; b5; b6; b7], ll)
            | _ -> failwith "not happening, protected"
          )
        else
          (
            let next = get_next_byte () in 
              match next with 
              | Some new_buffer -> first_8_bits_from_bits (buffer @ new_buffer)
              | None -> failwith "utf8 wrongly formatted"

          )
      in
      let rec aux buffer_left =
        let (nb_bytes, buffer_left) = n_bytes buffer_left in 

        let rec assemble_bytes k buffer_left acc =
          if k=0 then (acc, buffer_left)
          else 
            let (bits, buffer_left) = first_8_bits_from_bits buffer_left in 
            assemble_bytes (k-1) buffer_left (acc @ [(byte_of_bits bits)])
        in

        let (utf8_needed_bytes, buffer_left) = assemble_bytes nb_bytes buffer_left [] in 
        
        (* convertir en string pour Uutf *)
        let str = String.init nb_bytes (fun i -> Char.chr (List.nth utf8_needed_bytes i)) in
        let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
        match Uutf.decode decoder with
        | `Uchar uchar -> (uchar, buffer_left)
        | _ -> failwith "not a good utf8 format"
      in aux input_buffer
    in
    (* *)
    let find_next_code tree input_buffer =
      let rec aux t buffer_left =
        match t.content with 
        | Leaf (c, _) -> Some (c, buffer_left)
        | Node (e1, _, e2) -> 
          (
            match buffer_left with 
            | e :: ll -> 
              if e = 0 then aux e1 ll 
              else aux e2 ll
            | [] ->
              (
                let next = get_next_byte () in 
                match next with 
                | Some new_buffer -> 
                   (
                      match new_buffer with 
                      | ee :: llll -> 
                        if ee = 0 then aux e1 llll 
                        else aux e2 llll
                      | [] -> None
                   )
                | None -> None
              )
          )
      in
      aux tree input_buffer
    in
    (* *) 
    let output_encoder = Uutf.encoder `UTF_8 (`Channel out_channel) in
    let write_in_output uchar =
      if Uchar.to_int uchar = 0x0A then begin
        ignore (Uutf.encode output_encoder (`Uchar (Uchar.of_int 0x0D)));
      end;
      Uutf.encode output_encoder (`Uchar uchar);
    in

    let rec loop acc_table input_buffer =
      let hashtag_code = code (EmptyChar) acc_table in 
      (* check if the next bits match the # code *)
      let (is_next, input_buffer) = hashtag_is_next hashtag_code input_buffer in
      if (is_next) then
        (* read next UTF-8 : new char *)
        let (uchar, buffer_left) = read_next_utf8 input_buffer in 
        (* write character in output file *)
        ignore (write_in_output uchar);
        let acc_table = modification _H acc_table uchar in
        loop acc_table buffer_left
      else
        (* read a code in the tree until we find a leaf *)
        let uchar_opt = find_next_code _H input_buffer in
        (* write character in output file *)
        match uchar_opt with 
        | Some (uchar, buffer_left) -> 
          let uchar = (match uchar with Char u -> u | _ -> failwith "# cannot be written") in 
          (* uchar est ton Uchar.t *)
          ignore (write_in_output uchar);  
          let acc_table = modification _H acc_table uchar in
          loop acc_table buffer_left

        | None -> 
          (* Plus rien à lire *)
          (ignore (Uutf.encode output_encoder `End);
          close_in in_channel; 
          close_out out_channel;
          if visual then 
                    (btree_to_dot _H ("decompression_tree.dot");
                    Printf.printf "Decompression tree saved as decompression_tree.dot\n";))
    in
      (* skip BOM *) ignore (get_next_byte ()); ignore (get_next_byte ()); ignore (get_next_byte ());
      let bom = Uchar.of_int 0xFEFF in 
      let table = insert table bom in
      ignore (write_in_output bom);
      loop table [];
    )


  with 
    Sys_error _ -> 
            raise (Invalid_argument ("File not found: " ^ input_file ^ " or " ^ output_file))
    ;;

let () = 
    if Filename.basename Sys.argv.(0) = "decompression" then
    let args = Sys.argv in
    let input_file = args.(1) in
    let output_file = args.(2) in
    let visual = String.equal args.(3) "true" in
    decompression input_file output_file visual;