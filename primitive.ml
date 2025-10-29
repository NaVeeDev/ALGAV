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
                 | Some prev_v -> 
                  (prev_v <= List.hd x )
                 in
                 is_gd && is_bh && loop (Some (List.hd (List.rev x))) ll
  in
  loop None (List.rev lvl)

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

let insert (bTab : btreeTable) (c : char) : btreeTable = 
  match (CharaMap.find_opt (Char c) bTab) with 
  | None -> (*ajouter a la place de #*)
    let hashtag = CharaMap.find EmptyChar bTab in 
    let leaf1 = {content = Leaf(EmptyChar, 0)} in
    let leaf2 = {content = Leaf(Char c, 1)} in
    hashtag.content <- Node (leaf1, 1, leaf2);
    let bTab = CharaMap.add EmptyChar leaf1 bTab in
    let bTab = CharaMap.add (Char c) leaf2 bTab in
    bTab
  | Some btree -> (*incrémenter de 1*)
    match btree.content with 
    | Leaf (_, i) -> 
      btree.content <- Leaf(Char c, (i+1));
      bTab
    | _ -> failwith "shouldn't happen"
;;

let print_btree (t : btree) : unit = failwith "Not yet implemented"

let rec mem (c : chara) (m : btreeTable) : bool =
  CharaMap.mem c m

let update_weights (t : btree) : unit =
  let rec loop t =
    match t.content with 
    | Leaf (_,_) -> ()
    | Node (t1, _, t2) -> loop t1; loop t2;
                          let sum = match t1.content, t2.content with
                          | Node (_,v1, _), Node (_,v2, _)
                          | Leaf (_, v1), Node (_, v2,_) 
                          | Node (_, v1, _), Leaf (_, v2)
                          | Leaf (_, v1), Leaf (_, v2) -> v1 + v2
                          in
                          t.content <- Node (t1, sum, t2);
      in
    loop t

let switch (t1 : btree) (t2 : btree) =
  let temp = t1.content in 
  t1.content <- t2.content;
  t2.content <- temp

let finBloc (h : btree) (m : btree) : btree =
  let p = 
    match m.content with 
    | Leaf (_, p) | Node (_, p, _) -> p 
  in
  let rec loop btree k =
    match btree.content with 
    | Leaf (_, i) -> if i = p then Some (btree, k) else None 
    | Node (b1, i, b2) ->
      if i = p then Some (btree, k) else 
      let o1 = loop b1 (k+1) in 
      let o2 = loop b2 (k+1) in 
      match o1, o2 with 
      | None, None -> None
      | None, Some (a, b) | Some (a, b), None -> Some(a, b)
      | Some (a1, b1), Some (a2, b2) ->
        if b1 < b2 then Some (a1, b1) else 
        Some (a2, b2)
  in
  let o = loop h 0 in 
  match o with 
  | None -> failwith "isn't supposed to happen"
  | Some (res, _) -> res
      
let parent (t : btree) (t_child : btree) : btree =
  let rec loop curr =
    match curr.content with
    | Leaf (_, _) -> None
    | Node (l, _, r) ->
      if l == t_child || r == t_child then Some curr
      else match loop l with
      | None -> loop r
      | Some _ as res -> res
  in
  match loop t with
  | None -> failwith "No parent found"
  | Some p -> p


let chemin (t : btree) (t_end : btree) : btree list =
  let rec loop curr acc = 
    if curr == t_end then Some (curr :: acc)
    else
      match curr.content with
      | Leaf (_, _) -> None
      | Node (l, _, r) ->
        match loop l (curr :: acc) with
        | Some path -> Some path
        | None -> loop r (curr :: acc)
  in
  match loop t [] with
  | None -> failwith "No path found"
  | Some path -> path

let modification (_H : btree) (_table : btreeTable) (s : char) : btreeTable =
match _H.content with 
| Leaf (EmptyChar, _) -> 
  insert _table s
| _ ->
  if (not (mem (Char s) _table)) then 
    let _Q = parent _H (CharaMap.find EmptyChar _table) in 
    let _table = insert _table s in 
    traitement _H _Q
  else
    let _Q = CharaMap.find (Char s) _table in 
    let parent = parent _Q in 
    match parent.content with
    | Node ({content = Leaf(EmptyChar, _)}, _, _) -> (
      let finB = finBloc _H _Q in 
      if parent.content = finB.content then 
        ((match _Q.content with 
        | Node (e1, i, e2) -> _Q.content <- Node (e1, i + 1, e2)
        | _ -> failwith "shouldn't happen"
        );
        let _Q = parent in
        traitement _H _Q)
      else 
        traitement _H _Q
    )
    | _ -> traitement _H _Q
  