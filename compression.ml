open Primitive
let modification (h : btree) (t : btreeTable) (s:char) =
  match h.contents with 
  | Leaf(EmptyChar, 0) -> insert t s
  | 