(*##### FONCTIONS POUR LES LISTES ####*)

let rec is_sorted (l : int list) : bool =
  match l with
  | [] | [_] -> true
  | x :: x' :: ll -> x <= x' && is_sorted (x'::ll)


(*##### STRUCTURE POUR LES ARBRES BINAIRES ####*)

type chara = EmptyChar | Char of char
type btree_ = Leaf of chara * int | Node of btree * int * btree
and btree = {mutable content : btree_}

module CharaKey = struct
  type t = chara

  let compare (key1 : t) (key2 : t) = match key1, key2 with
  | EmptyChar, EmptyChar -> 0 (* On pourrait aussi faire crasher comme il ne peut pas y en avoir 2*)
  | EmptyChar, Char _ -> -1
  | Char _, EmptyChar -> 1
  | Char c1, Char c2 -> Char.compare c1 c2
end

module CharaMap = Map.Make(CharaKey)

type btreeTable = btree CharaMap.t 

(*##### FONCTIONS POUR LES ARBRES BINAIRES ####*)
let rec is_lte (t : btree) (v : int) : bool =
  match t.content with 
  | Leaf (_, i) -> i <= v
  | Node(t1, i, t2) -> i <= v && (is_lte t1 v) && (is_lte t2 v)

let rec is_gte (t : btree) (v : int) : bool = 
  match t.content with 
  | Leaf (_,i) -> i >= v 
  | Node (t1, i, t2) -> i >= v && (is_gte t1 v) && (is_gte t2 v)

let max_t (t : btree) : int =
  let rec loop t acc =
    match t.content with 
    | Leaf (_,i) -> if i > acc then i else acc
    | Node (t1, i, t2) -> if acc < i then loop t2 (loop t1 i) 
    else loop t2 (loop t1 acc)
  in
  loop t 0

 let vals_per_depth (t : btree) : int list list =
  let rec loop curr acc =
    match curr with
    | [] -> List.rev acc
    | _ ->
      let vals, next =
        List.fold_right (fun node (vals_acc, next_acc) ->
          match node.content with
          | Leaf (_, i) -> (i :: vals_acc, next_acc)
          | Node (l, i, r) -> (i :: vals_acc, l :: r :: next_acc)
        ) curr ([], [])
      in
      loop next (vals :: acc)
  in
  loop [t] []
;;

let is_gdbh (t : btree) : bool = 
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

let is_adding_up (t : btree) : bool =
  let rec loop t =
    match t.content with
    | Node (t1, i, t2) -> 
        let sum_children = match t1.content, t2.content with
          | Node (_,v1, _), Node (_,v2, _)
          | Leaf (_, v1), Node (_, v2,_) 
          | Node (_, v1, _), Leaf (_, v2)
          | Leaf (_, v1), Leaf (_, v2) -> v1 + v2
        in
        loop t1 && loop t2 && i = sum_children
    | Leaf (_,_) -> true
      in
  loop t

let insert (t : btree) (c : char) : btree = 
  let rec loop t =
    match t.content with
    | Leaf (EmptyChar, 0) -> Node (t, 1, {content = Leaf((Char c), 1)} )
    | Leaf (k,v) -> (match k with
       | Char (c) -> Leaf(k, v+1) 
       | _ -> t.content)
    | Node (t1, i, t2) -> Node ({content = loop t1}, i, {content = loop t2} )
  in
  {content = loop t}

let print_btree (t : btree) : unit = failwith "Not yet implemented"

let rec mem (c : chara) (m : btreeTable) : bool =
  CharaMap.mem c m

let update_weights (t : btree) : btree =
  let rec loop t =
    match t.content with 
    | Leaf (_,_) -> t
    | Node (t1, _, t2) -> let new_t1, new_t2 = loop t1, loop t2 in
                          let sum = match new_t1.content, new_t2.content with
                          | Node (_,v1, _), Node (_,v2, _)
                          | Leaf (_, v1), Node (_, v2,_) 
                          | Node (_, v1, _), Leaf (_, v2)
                          | Leaf (_, v1), Leaf (_, v2) -> v1 + v2
                          in
                          {content = Node (new_t1, sum, new_t2)}
      in
    loop t

let switch (t1 : btree) (t2 : btree) =
  let temp = t1.content in 
  t1.content <- t2.content;
  t2.content <- temp

let finBloc h m = failwith "TODO"