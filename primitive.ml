let rec is_sorted l =
  match l with
  | [] | [_] -> true
  | x :: x' :: ll -> x < x' && is_sorted (x'::ll)

type btree = Empty | Node of btree * (char * int) * btree

let rec is_lte t v =
  match t with 
  | Empty -> true
  | Node(t1, (_,i), t2) -> i <= v && (is_lte t1 v) && (is_lte t2 v)

let rec is_gte t v = 
  match t with 
  | Empty -> true 
  | Node (t1, (_,i), t2) -> i >= v && (is_gte t1 v) && (is_lte t2 v)

let max_t t =
  let rec loop t acc =
    match t with 
    | Empty -> acc
    | Node (t1, (_,i), t2) -> if acc < i then loop t2 (loop t1 i) 
    else loop t2 (loop t1 acc)
  in
  loop t 0

(*TODO : TESTS*)
let vals_per_depth t = 
  let rec loop acc curr =
    match curr with 
    | [] -> List.rev acc
    | _ -> let vals, next = List.fold_left (fun (v,n) -> function
                            | Empty -> (v,n)
                            | Node (t1, (_,i), t2) -> (i::v, n @ [t1; t2])) 
                            ([],[]) curr in
            loop (List.rev vals :: acc) next
  in
  loop [] [t]

let is_gdbh t = 
  let lvl = List.filter (fun l -> l <> []) (vals_per_depth t) in
  let rec loop last_of_prev_list l = 
    match l with 
    | [] -> true
    | x :: ll -> let is_gd = is_sorted x in
                 let is_bh = match last_of_prev_list with
                 | None -> true
                 | Some prev_v -> prev_v <= List.hd x in
                 is_gd && is_bh && loop (Some (List.hd (List.rev x))) ll
  in
  loop None lvl

