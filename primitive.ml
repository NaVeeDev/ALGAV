type btree = Empty | Node of btree * (char * int) * btree

let rec is_lte t v =
  match t with 
  | Empty -> true
  | Node(t1, (_,i), t2) -> if i > v then false else (is_lte t1 v) && (is_lte t2 v)

let rec is_gte t v = 
  match t with 
  | Empty -> true 
  | Node (t1, (_,i), t2) -> if i < v then false else (is_gte t1 v) && (is_lte t2 v)
