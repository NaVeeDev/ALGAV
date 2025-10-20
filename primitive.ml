(*##### FONCTIONS POUR LES LISTES ####*)

let rec is_sorted l =
  match l with
  | [] | [_] -> true
  | x :: x' :: ll -> x < x' && is_sorted (x'::ll)


(*##### STRUCTURE POUR LES ARBRES BINAIRES ####*)

type chara = EmptyChar | Char of char
type btree = Leaf of chara * int | Node of btree * int * btree

(*##### FONCTIONS POUR LES ARBRES BINAIRES ####*)
let rec is_lte t v =
  match t with 
  | Leaf (_, i) -> i <= v
  | Node(t1, i, t2) -> i <= v && (is_lte t1 v) && (is_lte t2 v)

let rec is_gte t v = 
  match t with 
  | Leaf (_,i) -> i >= v 
  | Node (t1, i, t2) -> i >= v && (is_gte t1 v) && (is_gte t2 v)

let max_t t =
  let rec loop t acc =
    match t with 
    | Leaf (_,i) -> if i > acc then i else acc
    | Node (t1, i, t2) -> if acc < i then loop t2 (loop t1 i) 
    else loop t2 (loop t1 acc)
  in
  loop t 0

let vals_per_depth t = 
  let rec loop acc curr =
    match curr with
    | [] -> List.rev acc
    | _ ->
        let vals, next = List.fold_left (fun (v,n) node ->
            match node with
            | Leaf (_, i) -> (i :: v, n)
            | Node (l, i, r) -> (i :: v, n @ [l; r])
          ) ([], []) curr in
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

let is_adding_up t =
  let rec loop t =
    match t with
    | Node (t1, i, t2) -> 
        let sum_children = match t1, t2 with
          | Node (_,v1, _), Node (_,v2, _)
          | Leaf (_, v1), Node (_, v2,_) 
          | Node (_, v1, _), Leaf (_, v2)
          | Leaf (_, v1), Leaf (_, v2) -> v1 + v2
        in
        loop t1 && loop t2 && i = sum_children
    | Leaf (_,_) -> true
      in
  loop t

let insert t c = 
  let rec loop t =
    match t with
    | Leaf (EmptyChar, 0) -> Node (t, 1, Leaf((Char c), 1))
    | Leaf (k,v) -> (match k with
       | Char (c) -> Leaf(k, v+1) 
       |_ -> t)
    | Node (t1, i, t2) -> Node (loop t1, i, loop t2)
  in
  loop t 

let print_btree t = failwith "Not yet implemented"

let rec mem t c =
  match t with 
  | Leaf (ch, _) -> (match ch with Char c' -> c' = c | _ -> false)
  | Node (t1,_, t2) -> mem t1 c || mem t2 c

let update_weights t =
  let rec loop t =
    match t with 
    | Leaf (_,_) -> t
    | Node (t1, _, t2) -> let new_t1, new_t2 = loop t1, loop t2 in
                          let sum = match new_t1, new_t2 with
                          | Node (_,v1, _), Node (_,v2, _)
                          | Leaf (_, v1), Node (_, v2,_) 
                          | Node (_, v1, _), Leaf (_, v2)
                          | Leaf (_, v1), Leaf (_, v2) -> v1 + v2
                          in
                          Node(new_t1, sum, new_t2)
      in
    loop t
