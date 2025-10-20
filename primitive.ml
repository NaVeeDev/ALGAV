(*##### FONCTIONS POUR LES LISTES ####*)

let rec is_sorted l =
  match l with
  | [] | [_] -> true
  | x :: x' :: ll -> x < x' && is_sorted (x'::ll)

type btree = Empty | Node of btree * (char option * int) * btree


(*##### FONCTIONS POUR LES ARBRES BINAIRES ####*)
let rec is_lte t v =
  match t with 
  | Empty -> true
  | Node(t1, (_,i), t2) -> i <= v && (is_lte t1 v) && (is_lte t2 v)

let rec is_gte t v = 
  match t with 
  | Empty -> true 
  | Node (t1, (_,i), t2) -> i >= v && (is_gte t1 v) && (is_gte t2 v)

let max_t t =
  let rec loop t acc =
    match t with 
    | Empty -> acc
    | Node (t1, (_,i), t2) -> if acc < i then loop t2 (loop t1 i) 
    else loop t2 (loop t1 acc)
  in
  loop t 0

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

let is_adding_up t =
  let rec loop t =
    match t with
    | Node (t1, (_,i), t2) -> 
        let sum_children = match t1, t2 with
          | Node (_,(_,v1), _), Node (_,(_,v2), _) -> v1 + v2
          | Node (_,(_,v1), _), Empty -> v1
          | Empty, Node (_,(_,v2), _) -> v2
          | Empty, Empty -> 0
        in
        i = sum_children && loop t1 && loop t2
    | Empty -> true
      in
  loop t

let insert t c = 
  let rec loop t =
    match t with
    | Empty -> Empty
    | Node (Empty, (Some '#', 0), Empty) ->
      let l = Node (Empty, (Some '#', 0), Empty) in
      let r = Node (Empty, (Some c, 1), Empty) in
      Node (l, (None, 1), r)
    | Node (t1, (k, i), t2) -> if k = Some c then raise (Invalid_argument "L'arbre contient déjà cette clé.") 
                               else Node(loop t1, (k,i), loop t2)
  in
  loop t                    