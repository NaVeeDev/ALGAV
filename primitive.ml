(*##### FONCTIONS POUR LES LISTES ####*)

let rec is_sorted (l : int list) : bool =
  match l with
  | [] | [_] -> true
  | x :: x' :: ll -> x <= x' && is_sorted (x'::ll)


(*##### STRUCTURE POUR LES ARBRES BINAIRES ####*)

type chara = EmptyChar | Char of Uchar.t
type btree_ = Leaf of chara * int | Node of btree * int * btree
and btree = {mutable content : btree_; mutable parent : btree option}

module CharaKey = struct
  type t = chara

  let compare (key1 : t) (key2 : t) = match key1, key2 with
  | EmptyChar, EmptyChar -> 0 (* On pourrait aussi faire crasher comme il ne peut pas y en avoir 2*)
  | EmptyChar, Char _ -> -1
  | Char _, EmptyChar -> 1
  | Char c1, Char c2 -> Uchar.compare c1 c2
end

module CharaMap = Map.Make(CharaKey)

type btreeTable = btree CharaMap.t 

let uchar_to_string (u : Uchar.t) : string =
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b u;
  Buffer.contents b

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
    | [] -> acc
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

let node_per_depth (t : btree ) : btree list list =
  let rec loop curr acc =
    match curr with
    | [] -> acc
    | _ ->
      let vals, next =
        List.fold_right (fun node (vals_acc, next_acc) ->
          match node.content with
          | Leaf _ -> (node :: vals_acc, next_acc)
          | Node (l, _, r) -> (node :: vals_acc, l :: r :: next_acc)
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

let print_btree (t : btree) : unit = 
  let rec to_string btree = 
    match btree.content with 
    | Leaf (EmptyChar, i) -> "Leaf(#, " ^ (Int.to_string i) ^ ")"
    | Leaf (Char c, i) -> 
      let res = "Leaf(" ^ (uchar_to_string c) ^ ", " ^ (Int.to_string i) ^ ")" in res 
    | Node (e1, i, e2) ->
      let s1 = to_string e1 in 
      let s2 = to_string e2 in 
      "Node("^s1^", " ^ (Int.to_string i) ^ ", " ^s2^")"
  in 
  Printf.printf "%s\n" (to_string t)

let insert (bTab : btreeTable) (c : Uchar.t) : btreeTable = 
  match (CharaMap.find_opt (Char c) bTab) with 
  | None -> (*ajouter a la place de #*)
    let hashtag = CharaMap.find EmptyChar bTab in
    let leaf1 = { content = Leaf (EmptyChar, 0); parent = None } in
    let leaf2 = { content = Leaf (Char c, 1); parent = None } in
    hashtag.content <- Node (leaf1, 1, leaf2);
    leaf1.parent <- Some hashtag;
    leaf2.parent <- Some hashtag;
    let bTab = CharaMap.add EmptyChar leaf1 bTab in
    let bTab = CharaMap.add (Char c) leaf2 bTab in
    bTab
  | Some btree -> (*incrémenter de 1*)
    match btree.content with 
    | Leaf (_, i) -> 
      btree.content <- Leaf(Char c, (i+1));
      bTab
    | _ -> failwith "shouldn't happen1"
;;

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

let switch (table : btreeTable) (t1 : btree) (t2 : btree) : btreeTable =
  (* on échange les contenus *)
  let temp = t1.content in
  t1.content <- t2.content;
  t2.content <- temp;

  let update_parents t =
    match t.content with
    | Node (l, _, r) ->
        l.parent <- Some t;
        r.parent <- Some t
    | _ -> ()
  in
  update_parents t1;
  update_parents t2;

  let table =
    match t1.content, t2.content with
    | Leaf (c1, _), Leaf (c2, _) ->
        let table = CharaMap.add c1 t1 table in
        let table = CharaMap.add c2 t2 table in
        table
    | Leaf (c1, _), Node (e1, _, e2) ->
        let table = CharaMap.add c1 t1 table in
        table
    | Node (e1, _, e2), Leaf (c2, _) ->
        let table = CharaMap.add c2 t2 table in
        table
    | Node (_, _, _), Node (_, _, _) ->
        table
  in
  table
;;

let btree_to_dot (h : btree) (filename : string) : unit =
  let oc = open_out filename in
  let rec write_node oc node =
    match node.content with
    | Leaf (c, w) ->
        let label = match c with
          | EmptyChar -> "#"
          | Char u -> uchar_to_string u
        in
        Printf.fprintf oc "  \"%d\" [label=\"%s, %d\", shape=ellipse, style=filled,fillcolor=lightgreen];\n" (Hashtbl.hash node) label w
    | Node (left, w, right) ->
        Printf.fprintf oc "  \"%d\" [label=\"%d\", shape=plaintext];\n" (Hashtbl.hash node) w;
        write_node oc left;
        write_node oc right;
        Printf.fprintf oc "  \"%d\" -> \"%d\" [label=\"0\"];\n" (Hashtbl.hash node) (Hashtbl.hash left);
        Printf.fprintf oc "  \"%d\" -> \"%d\" [label=\"1\"];\n" (Hashtbl.hash node) (Hashtbl.hash right);
  in
  Printf.fprintf oc "digraph G {\n";
  write_node oc h;
  Printf.fprintf oc "}\n";
  close_out oc;;


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

let rec equal_btree (t1 : btree) (t2 : btree) : bool =
  t1.content == t2.content
      
let parent (t : btree) : btree =
  match t.parent with 
  | None -> raise Not_found
  | Some node -> node


let chemin (t_end : btree) : btree list =
  let rec loop curr =
    match curr.parent with 
    | None -> [curr]
    | Some p -> 
      curr :: (loop p)
  in
  loop t_end


let is_incrementable (t : btree) (l : btree list) : bool * btree option =
  let npd = node_per_depth t in
  let find_next_elem (target : btree) : btree option = 
    let rec loop npd_ =
      match npd_ with 
      | current_npd :: npd_after -> (
        let rec looper l = (
          match l with 
          | e :: ee :: ll -> 
            (* ee est le next si e est target, sinon on continue la recherche *)
            if equal_btree e target then Some ee
            else looper (ee :: ll)
          | e :: [] -> 
            (* si target est au bout de la liste, le next est le premier elem de la profondeur d'au dessus *)
            if equal_btree e target then
              (
                match npd_after with
                | list :: _ -> (
                  match list with 
                  | e :: _ -> Some e 
                  | _ -> failwith "Not supposed to happen 1"
                )
                | _ -> None
              )
            (* target n'est pas dans cette profondeur, on cherche dans les profondeurs plus hautes *)
            else None
          | [] -> None 
        )
        in 
        (match looper current_npd with 
          | None -> loop npd_after
          | Some res -> Some res)
      )
      | _ -> None
    in 
    loop npd
  in
  let rec check_node l =
    match l with 
    | e :: ll -> (
      let next = find_next_elem e in 
      match next with 
      | None -> (true, None)
      | Some next -> (
        match next.content, e.content with 
        | Node (_, highest, _), Node (_, lowest, _) | Node (_, highest, _), Leaf (_, lowest)
        | Leaf (_, highest), Node (_, lowest, _) | Leaf (_, highest), Leaf (_, lowest) ->
          if lowest < highest then 
            check_node ll
          else
            (false, Some e)
      )
    )
    | [] -> (true, None)
  in
  check_node l

let code (chara : chara) (table : btreeTable) : int list =
  let node = CharaMap.find chara table in 
  let rec loop n acc =
    match n.parent with 
    | None -> acc 
    | Some p -> 
      (match p.content with
      | Node (e1, _, e2) -> 
        let code = (if equal_btree e1 n then 0 else 1) in 
        loop p (code :: acc)
      | _ -> failwith "not happening"
      )
  in
  loop node []

let rec traitement (_H : btree) (_Q : btree) (t : btreeTable) : btreeTable =
  let chemin = chemin _Q in
  let (is_incrementable, m_option) = is_incrementable _H chemin in
  if is_incrementable then 
    let rec increment l =
      match l with 
      | [] -> ()
      | x :: ll -> match x.content with 
        | Node (e1, i, e2) -> x.content <- Node (e1, i + 1, e2); increment ll
        | Leaf (_, i ) -> x.content <- Leaf (Char (match x.content with Leaf (Char c, _) -> c | _ -> failwith "shouldn't happen2"), i + 1); increment ll
    in
    increment chemin;
    update_weights _H;
    t
  else
    let m = match m_option with | None -> failwith "non sense" | Some e -> e in
    let b = finBloc _H m in
    let increment_until l stop =
      let rec loop lst =
        match lst with
        | [] -> ()
        | x :: ll -> if equal_btree x stop then
              (match x.content with 
            | Node (e1, i, e2) -> x.content <- Node (e1, i + 1, e2); 
            | Leaf (_, i ) -> x.content <- Leaf (Char (match x.content with Leaf (Char c, _) -> c | _ -> failwith "shouldn't happen5"), i + 1))
          else (match x.content with 
            | Node (e1, i, e2) -> x.content <- Node (e1, i + 1, e2); loop ll
            | Leaf (_, i ) -> x.content <- Leaf (Char (match x.content with Leaf (Char c, _) -> c | _ -> failwith "shouldn't happen5"), i + 1); loop ll)
      in
      loop l
    in
    increment_until chemin m;
    let t = switch t b m in
    (* ici on utilise b plutot que m car b et m ont été switch (mais pas leur nom local, seulement les contenus) *)
    traitement _H (parent b) t

let modification (_H : btree) (_table : btreeTable) (s : Uchar.t) : btreeTable =
match _H.content with 
| Leaf (EmptyChar, _) -> 
  (* H == # *)
  insert _table s
| _ ->
  if (not (mem (Char s) _table)) then 
    (* s not in H *)
    let _Q = parent (CharaMap.find EmptyChar _table) in 
    let _table = insert _table s in
    traitement _H _Q _table
  else
    (* else *)
    let _Q = CharaMap.find (Char s) _table in 
    let parent = parent _Q in 
    match parent.content with
    | Node ({content = Leaf(EmptyChar, _)}, _, _) -> (
      let finB = finBloc _H _Q in 
      if equal_btree parent finB then 
        (* if ( enfants(parent(Q)) == {#, Q} and parent(Q) == finBloc(H,Q) ) *)
        (
          (match _Q.content with
          | Leaf (c, i) -> _Q.content <- Leaf (c, i+1)
          | Node (e1, i, e2) -> _Q.content <- Node (e1, i+1, e2)
          ); 
          let _Q = parent in
          traitement _H _Q _table
        )
      else 
        traitement _H _Q _table
    )
    | _ -> traitement _H _Q _table
  

(*##### FONCTION POUR LE CODE INITIAL ####*)


let initial_code (s : Uchar.t) : int list =
  let buffer = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 buffer s;
  let str = Buffer.contents buffer in

  let bits_of_byte byte =
    let rec aux n acc =
      if n < 0 then acc
      else aux (n-1) (Utils.nth_bit n byte :: acc)
    in
    aux 7 []
  in
  let rec loop i acc =
    if i >= String.length str then acc
    else loop (i+1) (acc @ bits_of_byte (Char.code str.[i]))
  in
  loop 0 []
;;
