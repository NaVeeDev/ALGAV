(*##### FONCTIONS POUR LES LISTES ####*)

let test_is_sorted_ () =
  assert (Primitive.is_sorted []);
  assert (Primitive.is_sorted [0]);
  assert (Primitive.is_sorted [-1]);
  assert (Primitive.is_sorted [15646]);
  assert (Primitive.is_sorted [0;1;2;3;4]);
  assert (Primitive.is_sorted [-8;-2;16]);
  assert (Primitive.is_sorted [0; 0]);
  assert (Primitive.is_sorted [1; 1; 2; 2; 2; 3; 3; 3; 3]);
  assert (not (Primitive.is_sorted [4; 3; 2; 1]));
  assert (not (Primitive.is_sorted [-4; -5]));
  assert (not (Primitive.is_sorted [28; 29; 48; 2]));
  assert (not (Primitive.is_sorted [49; 1; 2; 3; 4]));
;;
(*##### FONCTIONS POUR LES ARBRES BINAIRES ####*)
open Primitive

let create_leaf chara i = { content = Leaf (chara, i) }
let create_node b1 i b2 = { content = Node (b1, i, b2) }


let test_is_lte_ () =
  let emptyLeaf = create_leaf EmptyChar 0 in 
  assert (is_lte emptyLeaf 0);
  assert (is_lte emptyLeaf 100);
  assert (not (is_lte emptyLeaf (-100)));

  let negativeLeaf = create_leaf EmptyChar (-1000) in 
  assert (is_lte negativeLeaf 10);
  assert (is_lte negativeLeaf (-1000));
  assert (not (is_lte negativeLeaf (-4125)));

  let positiveLeaf = create_leaf EmptyChar 1692 in 
  assert (is_lte positiveLeaf 1692);
  assert (not (is_lte positiveLeaf (-1340)));
  assert (is_lte positiveLeaf 11564);

  let tree1 = create_node emptyLeaf 0 emptyLeaf in 
  assert (is_lte tree1 0);
  assert (is_lte tree1 100);
  assert (not (is_lte tree1 (-100)));
  
  let tree1_expanded =
    create_node
      (create_node emptyLeaf 0 (create_node emptyLeaf 0 emptyLeaf))
      0
      (create_node
         (create_node
            (create_node
               (create_node emptyLeaf 0 emptyLeaf)
               0
               emptyLeaf
            )
            0
            (create_node emptyLeaf 0 emptyLeaf)
         )
         0
         (create_node emptyLeaf 0 emptyLeaf)
      )
  in
  assert (is_lte tree1_expanded 0);
  assert (is_lte tree1_expanded 100);
  assert (not (is_lte tree1_expanded (-100)));

  let tree2 =
    create_node
      (create_node (create_leaf EmptyChar 100) 4 (create_leaf EmptyChar 130))
      67
      (create_node
         (create_node (create_leaf EmptyChar 180) 148
            (create_node (create_leaf EmptyChar 170) 138
               (create_node
                  (create_node (create_leaf EmptyChar 128) 68 (create_leaf EmptyChar 201))
                  200
                  emptyLeaf
               )
            )
         )
         49
         (create_leaf EmptyChar 120)
      )
  in
  assert (is_lte tree2 201);
  assert (is_lte tree2 202);
  assert (not (is_lte tree2 199));
  assert (not (is_lte tree2 0));
;;


let test_is_gte_ () =
  let emptyLeaf = create_leaf EmptyChar 0 in 
  assert (is_gte emptyLeaf 0);
  assert (is_gte emptyLeaf (-100));
  assert (not (is_gte emptyLeaf 100));

  let negativeLeaf = create_leaf EmptyChar (-1000) in 
  assert (not (is_gte negativeLeaf 10));
  assert (is_gte negativeLeaf (-1000));
  assert (is_gte negativeLeaf (-4125));

  let positiveLeaf = create_leaf EmptyChar 1692 in 
  assert (is_gte positiveLeaf 1692);
  assert (is_gte positiveLeaf (-1340));
  assert (not (is_gte positiveLeaf 11564));

  let tree1 = create_node emptyLeaf 0 emptyLeaf in 
  assert (is_gte tree1 0);
  assert (not (is_gte tree1 100));
  assert (is_gte tree1 (-100));
  
  let tree1_expanded =
    create_node
      (create_node emptyLeaf 0 (create_node emptyLeaf 0 emptyLeaf))
      0
      (create_node
         (create_node
            (create_node
               (create_node emptyLeaf 0 emptyLeaf)
               0
               emptyLeaf
            )
            0
            (create_node emptyLeaf 0 emptyLeaf)
         )
         0
         (create_node emptyLeaf 0 emptyLeaf)
      )
  in
  assert (is_gte tree1_expanded 0);
  assert (not (is_gte tree1_expanded 100));
  assert (is_gte tree1_expanded (-100));

  let tree2 =
    create_node
      (create_node (create_leaf EmptyChar 100) 4 (create_leaf EmptyChar 130))
      67
      (create_node
         (create_node (create_leaf EmptyChar 180) 148
            (create_node (create_leaf EmptyChar 170) 138
               (create_node
                  (create_node (create_leaf EmptyChar 128) 68 (create_leaf EmptyChar 201))
                  200
                  (create_leaf (Char 'c') 100)
               )
            )
         )
         49
         (create_leaf EmptyChar 120)
      )
  in
  assert (not (is_gte tree2 201));
  assert (not (is_gte tree2 5));
  assert (is_gte tree2 4);
  assert (is_gte tree2 0);
;;

let test_max_t_ () =
   ()
;;

let test_finBloc_ () =
  let leaf = create_leaf (Char 'x') 10 in
  let result1 = finBloc leaf leaf in
  (match result1.content with
  | Leaf (Char ch, v) ->
      assert (ch = 'x');
      assert (v = 10)
  | _ -> assert false);

  let left = create_leaf EmptyChar 3 in
  let right = create_leaf (Char 'c') 3 in
  let root = create_node left 6 right in

  let result = finBloc root left in

  (match result.content with
  | Leaf (c, v) -> assert (v = 3);
                   (match c with 
                   | EmptyChar -> assert false
                   | Char ch -> assert (ch = 'c'))
  | _ -> assert false);
  
  (*
       (_ , 15)
       /      \
   (_, 10) (res, 5)
     / \     /   \
   (5) (5) (c,3) (d,2)
  *)
  let l1 = create_leaf (Char 'a') 5 in
  let l2 = create_leaf (Char 'b') 5 in
  let left_big = create_node l1 10 l2 in

  let l3 = create_leaf (Char 'c') 3 in
  let l4 = create_leaf (Char 'd') 2 in
  let right_big = create_node l3 5 l4 in

  let root_big = create_node left_big 10 right_big in

  
  let result = finBloc root_big l1 in

  (match result.content with
  | Leaf _ -> assert false;
  | Node(_,i,_) -> assert (i=5));

   (*
                   (root, 45)
                 /            \
          (n1, 15)           (n2, 30)
          /      \           /      \
     (n3, 15)  (n4, 0)  (n5, 15)  (n6, 15)
      /   \     /   \     /   \    /   \
    (7)   (8) (0)  (0)  (6)  (9) (0)  (15)
   *)

  let l1 = create_leaf (Char 'a') 7 in
  let l2 = create_leaf (Char 'b') 8 in
  let l3 = create_leaf (Char 'c') 0 in
  let l4 = create_leaf (Char 'd') 0 in
  let l5 = create_leaf (Char 'e') 6 in
  let l6 = create_leaf (Char 'f') 9 in
  let l7 = create_leaf (Char 'g') 0 in
  let l8 = create_leaf (Char 'h') 15 in

  let n3 = create_node l1 15 l2 in
  let n4 = create_node l3 0 l4 in
  let n5 = create_node l5 15 l6 in
  let n6 = create_node l7 15 l8 in

  let n1 = create_node n3 15 n4 in
  let n2 = create_node n5 30 n6 in

  let root = create_node n1 45 n2 in

  let result = finBloc root l7 in
  (match result.content with
  | Leaf _ -> assert false
  | Node(_,i,_) -> assert (i=0));

  let result = finBloc root l3 in
  (match result.content with
  | Leaf _ -> assert false
  | Node(_,i,_) -> assert (i=0));

  let result = finBloc root l4 in
  (match result.content with
  | Leaf _ -> assert false
  | Node(_,i,_) -> assert (i=0));

  let result = finBloc root n4 in
  (match result.content with
  | Leaf _ -> assert false
  | Node(_,i,_) -> assert (i=0));



  let result = finBloc root l8 in
  (match result.content with
  | Leaf _ -> assert false
  | node -> assert (node == n1.content));

  let result = finBloc root n1 in
  (match result.content with
  | Leaf _ -> assert false
  | node -> assert (node == n1.content));

   let result = finBloc root n3 in
  (match result.content with
  | Leaf _ -> assert false
  | node -> assert (node == n1.content));

  let result = finBloc root n5 in
  (match result.content with
  | Leaf _ -> assert false
  | node -> assert (node == n1.content));

  let result = finBloc root n6 in
  (match result.content with
  | Leaf _ -> assert false
  | node -> assert (node == n1.content));
  ()
;;


(* Appeler cette fonction pour executer les tests *)
let test_primitive_ () =
  Printf.printf "\n___ Lancement des tests de test_primitive.ml ___\n";
  try
    test_is_sorted_ ();
    test_is_lte_ ();
    test_is_gte_ ();
    test_max_t_ ();
    test_finBloc_ ();
    
    Printf.printf "___ Tous les tests de test_primitive.ml sont passés ___\n"
  with e -> Printf.printf "_Un test de test_primitive.ml n'est pas passé : \n%s\n" (Printexc.to_string e);