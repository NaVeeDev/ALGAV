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

let create_leaf chara i = { content = Leaf (chara, i); parent = None }
let create_node b1 i b2 = { content = Node (b1, i, b2); parent = None }


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
                  (create_leaf (Char (Uchar.of_char 'c')) 100)
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

let test_update_weights_ () =
   let l1 = create_leaf (Char (Uchar.of_char 'a')) 1 in 
   let l2 = create_leaf (Char (Uchar.of_char 'b')) 2 in 
   let l3 = create_leaf (Char (Uchar.of_char 'c')) 3 in 
   let l4 = create_leaf (Char (Uchar.of_char 'd')) 4 in 

   let l5 = create_leaf (Char (Uchar.of_char 'e')) 5 in 
   let l6 = create_leaf (Char (Uchar.of_char 'f')) 6 in 
   let l7 = create_leaf (Char (Uchar.of_char 'g')) 7 in 
   let l8 = create_leaf (Char (Uchar.of_char 'h')) 8 in
   
   let n4 = create_node l1 (-5) l2 in 
   let n5 = create_node l3 (-5) l4 in 
   let n6 = create_node l5 (-5) l6 in 
   let n7 = create_node l7 (-5) l8 in

   let n2 = create_node n4 (-12) n5 in
   let n3 = create_node n6 (-12) n7 in

   let n1 = create_node n2 (-12) n3 in

   update_weights n1;

   (match n1.content with 
   | Leaf _ -> assert false;
   | Node (e1, p, e2) ->
      assert (p = 36);
      (match e1.content with 
      | Node (e1, p, e2) -> 
         assert (p = 10);
         (match e1.content with
         | Node (e1, p, e2) -> 
            assert (p = 3);
            assert (e1 == l1);
            assert (e2 == l2);
         | _ -> assert false;
         );
         (match e2.content with 
         | Node (e1, p, e2) -> 
            assert (p = 7);
            assert (e1 == l3);
            assert (e2 == l4);
         | _ -> assert false;
         );
      | _ -> assert false;
      );
      (match e2.content with 
      | Node (e1, p, e2) -> 
         assert (p = 26);
         (match e1.content with
         | Node (e1, p, e2) -> 
            assert (p = 11);
            assert (e1 == l5);
            assert (e2 == l6);
         | _ -> assert false;
         );
         (match e2.content with 
         | Node (e1, p, e2) -> 
            assert (p = 15);
            assert (e1 == l7);
            assert (e2 == l8);
         | _ -> assert false;
         );
      | _ -> assert false;
      );
   );
   ()
;;

let test_switch_ () =
   let tree = {content = Leaf (EmptyChar, 0); parent = None} in 
   let table = CharaMap.empty in 
   let table = CharaMap.add EmptyChar tree table in

   let table = modification tree table (Uchar.of_char 'c') in
   let table = modification tree table (Uchar.of_char 'a') in 
   let table = modification tree table (Uchar.of_char 'r') in
   let table = modification tree table (Uchar.of_char 'a') in 
   let table = modification tree table (Uchar.of_char 'm') in 
   let table = modification tree table (Uchar.of_char 'b') in 
   let table = modification tree table (Uchar.of_char 'a') in 
   let table = modification tree table (Uchar.of_char 'r') in 
   let table = modification tree table (Uchar.of_char 'b') in 
   let table = modification tree table (Uchar.of_char 'c') in 
   let table = modification tree table (Uchar.of_char 't') in 
   let _ = switch table (parent (CharaMap.find (Char (Uchar.of_char 'r')) table)) (parent (CharaMap.find (Char (Uchar.of_char 'm')) table)) in
   Printf.printf "Supposed to be : Node(Node(Node(Leaf(#, 0), 1, Leaf(t, 1)), 2, Leaf(m, 1)), 11, Node(Leaf(a, 3), 7, Node(Node(Leaf(b, 2), 4, Leaf(r, 2)), 4, Leaf(c, 2))))"; print_newline ();
   Printf.printf "Actual         : "; print_btree tree; print_newline ();
;;

let test_finBloc_ () =
  let leaf = create_leaf (Char (Uchar.of_char 'x')) 10 in
  let result = finBloc leaf leaf in
  assert (result == leaf);

  let left = create_leaf EmptyChar 3 in
  let right = create_leaf (Char (Uchar.of_char 'c')) 3 in
  let root = create_node left 6 right in

  let result = finBloc root left in

  assert (result == right);
  
  (*
       (_ , 15)
       /      \
   (_, 10) (res, 5)
     / \     /   \
   (5) (5) (c,3) (d,2)
  *)
  let l1 = create_leaf (Char (Uchar.of_char 'a')) 5 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 5 in
  let left_big = create_node l1 10 l2 in

  let l3 = create_leaf (Char (Uchar.of_char 'c')) 3 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 2 in
  let right_big = create_node l3 5 l4 in

  let root_big = create_node left_big 10 right_big in

  
  let result = finBloc root_big l1 in

  assert (result == right_big);

   (*
                   (root, 45)
                 /            \
          (n1, 15)           (n2, 30)
          /      \           /      \
     (n3, 15)  (n4, 0)  (n5, 15)  (n6, 15)
      /   \     /   \     /   \    /   \
    (7)   (8) (0)  (0)  (6)  (9) (0)  (15)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 7 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 8 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 0 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 0 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 6 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 9 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 0 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 15 in

  let n3 = create_node l1 15 l2 in
  let n4 = create_node l3 0 l4 in
  let n5 = create_node l5 15 l6 in
  let n6 = create_node l7 15 l8 in

  let n1 = create_node n3 15 n4 in
  let n2 = create_node n5 30 n6 in

  let root = create_node n1 45 n2 in

  let result = finBloc root l7 in
  assert (result == n4);

  let result = finBloc root l3 in
  assert (result == n4);

  let result = finBloc root l4 in
  assert (result == n4);

  let result = finBloc root n4 in
  assert (result == n4);

  let result = finBloc root l8 in
  assert (result == n1);

  let result = finBloc root n1 in
  assert (result == n1);

  let result = finBloc root n3 in
  assert (result == n1);

  let result = finBloc root n5 in
  assert (result == n1);

  let result = finBloc root n6 in
  assert (result == n1);
;;


let test_insert_ () =
   let tree = {content = Leaf (EmptyChar, 0); parent = None} in 
   let table = CharaMap.empty in 
   let table = CharaMap.add EmptyChar tree table in
   
   let table = insert table (Uchar.of_char 'a') in 
   assert (mem (Char (Uchar.of_char 'a')) table);
   
   let o = CharaMap.find_opt (Char (Uchar.of_char 'a')) table in 
   (match o with 
   | None -> assert false;
   | Some btree -> 
         assert (equal_btree tree (parent btree));
         (match btree.content with 
                   | Leaf (chara, i) -> (match chara with 
                                   | EmptyChar -> assert false; 
                                   | Char c -> assert (c = (Uchar.of_char 'a'))
                                   );
                                   assert (i=1)
                   | _ -> assert false;
   ));
   (match tree.content with
   | Leaf _ -> assert false
   | Node (e1, _, e2) -> (match e1.content with
                         | Leaf (chara, i) -> (match chara with 
                                         | EmptyChar -> assert true; 
                                         | _ -> assert false;
                                         );
                                         assert (i=0)
                         | _ -> assert false;
                         );
                         (match e2.content with
                         | Leaf (chara, i) -> (match chara with 
                                         | EmptyChar -> assert false; 
                                         | Char c -> assert (c = (Uchar.of_char 'a'))
                                         );
                                         assert (i=1)
                         | _ -> assert false;
                         );
   );
   
   let table = insert table (Uchar.of_char 'a') in 
   assert (mem (Char (Uchar.of_char 'a')) table);
   let o = CharaMap.find_opt (Char (Uchar.of_char 'a')) table in 
   (match o with 
   | None -> assert false;
   | Some btree -> (match btree.content with 
                   | Leaf (chara, i) -> (match chara with 
                                   | EmptyChar -> assert false; 
                                   | Char c -> assert (c = (Uchar.of_char 'a'))
                                   );
                                   assert (i=2)
                   | _ -> assert false;
   ));
   (match tree.content with
   | Leaf _ -> assert false
   | Node (e1, _, e2) -> (match e1.content with
                         | Leaf (chara, i) -> (match chara with 
                                         | EmptyChar -> assert true; 
                                         | _ -> assert false;
                                         );
                                         assert (i=0)
                         | _ -> assert false;
                         );
                         (match e2.content with
                         | Leaf (chara, i) -> (match chara with 
                                         | EmptyChar -> assert false; 
                                         | Char c -> assert (c = (Uchar.of_char 'a'))
                                         );
                                         assert (i=2)
                         | _ -> assert false;
                         );
   );

   let table = insert table (Uchar.of_char 'b') in 
   assert (mem (Char (Uchar.of_char 'a')) table);
   assert (mem (Char (Uchar.of_char 'b')) table);
   let o = CharaMap.find_opt (Char (Uchar.of_char 'a')) table in 
   (match o with 
   | None -> assert false;
   | Some btree -> (match btree.content with 
                   | Leaf (chara, i) -> (match chara with 
                                   | EmptyChar -> assert false; 
                                   | Char c -> assert (c = (Uchar.of_char 'a'))
                                   );
                                   assert (i=2)
                   | _ -> assert false;
   ));
   let o = CharaMap.find_opt (Char (Uchar.of_char 'b')) table in 
   (match o with 
   | None -> assert false;
   | Some btree -> (match btree.content with 
                   | Leaf (chara, i) -> (match chara with 
                                   | EmptyChar -> assert false; 
                                   | Char c -> assert (c = (Uchar.of_char 'b'))
                                   );
                                   assert (i=1)
                   | _ -> assert false;
   ));
   (match tree.content with
   | Leaf _ -> assert false
   | Node (e1, _, e2) -> (match e2.content with
                         | Leaf (chara, i) -> (match chara with 
                                         | EmptyChar -> assert false; 
                                         | Char c -> assert (c = (Uchar.of_char 'a'));
                                         );
                                         assert (i=2)
                         | _ -> assert false;
                         );
                         (match e1.content with
                         | Leaf _ -> assert false;
                         | Node (e1, _, e2) -> 
                           (match e1.content with
                           | Leaf (chara, i) -> (match chara with 
                                                | EmptyChar -> assert true;
                                                | _ -> assert false;
                                                );
                                                assert (i=0);
                           | _ -> assert false;
                           );
                           (match e2.content with
                           | Leaf (chara, i) -> (match chara with 
                                          | EmptyChar -> assert false; 
                                          | Char c -> assert (c = (Uchar.of_char 'b'))
                                          );
                                          assert (i=1)
                           | _ -> assert false;
                           );
                         );
   );

   let table = insert table (Uchar.of_char 'b') in 
   assert (mem (Char (Uchar.of_char 'a')) table);
   assert (mem (Char (Uchar.of_char 'b')) table);
   let o = CharaMap.find_opt (Char (Uchar.of_char 'a')) table in 
   (match o with 
   | None -> assert false;
   | Some btree -> (match btree.content with 
                   | Leaf (chara, i) -> (match chara with 
                                   | EmptyChar -> assert false; 
                                   | Char c -> assert (c = (Uchar.of_char 'a'))
                                   );
                                   assert (i=2)
                   | _ -> assert false;
   ));
   let o = CharaMap.find_opt (Char (Uchar.of_char 'b')) table in 
   (match o with 
   | None -> assert false;
   | Some btree -> (match btree.content with 
                   | Leaf (chara, i) -> (match chara with 
                                   | EmptyChar -> assert false; 
                                   | Char c -> assert (c = (Uchar.of_char 'b'))
                                   );
                                   assert (i=2)
                   | _ -> assert false;
   ));
   (match tree.content with
   | Leaf _ -> assert false
   | Node (e1, _, e2) -> (match e2.content with
                         | Leaf (chara, i) -> (match chara with 
                                         | EmptyChar -> assert false; 
                                         | Char c -> assert (c = (Uchar.of_char 'a'));
                                         );
                                         assert (i=2)
                         | _ -> assert false;
                         );
                         (match e1.content with
                         | Leaf _ -> assert false;
                         | Node (e1, _, e2) -> 
                           (match e1.content with
                           | Leaf (chara, i) -> (match chara with 
                                                | EmptyChar -> assert true;
                                                | _ -> assert false;
                                                );
                                                assert (i=0);
                           | _ -> assert false;
                           );
                           (match e2.content with
                           | Leaf (chara, i) -> (match chara with 
                                          | EmptyChar -> assert false; 
                                          | Char c -> assert (c = (Uchar.of_char 'b'))
                                          );
                                          assert (i=2);
                           | _ -> assert false;
                           );
                         );
   );
   ()
;;


let test_is_adding_up_ () = 
   let tree = {content = Leaf (EmptyChar, 0); parent = None} in 
   let table = CharaMap.empty in 
   let table = CharaMap.add EmptyChar tree table in

   for i = 0 to 2 do
      let _ = insert table (Uchar.of_char 'a') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'a') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'b') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'b') in 
      update_weights tree;
      assert (is_adding_up tree);
      
      let _ = insert table (Uchar.of_char 'a') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'c') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'd') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'd') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'a') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'e') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'f') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'e') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'g') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'h') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'o') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'd') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'c') in 
      update_weights tree;
      assert (is_adding_up tree);

      let _ = insert table (Uchar.of_char 'd') in 
      update_weights tree;
      assert (is_adding_up tree);
   done;
   ()
;;

let test_is_gdbh_ () =
   (*
                   (root, 256)
                 /            \
          (n1, 128)           (n2, 128)
          /      \           /      \
     (n3, 64)  (n4, 64)  (n5, 64) (n6, 64)
      /   \     /   \     /   \    /   \
    (32) (32) (32)  (32) (32) (32)(32) (32)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 32 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 32 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 32 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 32 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 32 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 32 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 32 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 32 in

  let n3 = create_node l1 64 l2 in
  let n4 = create_node l3 64 l4 in
  let n5 = create_node l5 64 l6 in
  let n6 = create_node l7 64 l8 in

  let n1 = create_node n3 128 n4 in
  let n2 = create_node n5 128 n6 in

  let root = create_node n1 256 n2 in

  assert (is_gdbh root);


   (*
                   (root, 45)
                 /            \
          (n1, 15)           (n2, 30)
          /      \           /      \
     (n3, 15)  (n4, 0)  (n5, 15)  (n6, 15)
      /   \     /   \     /   \    /   \
    (7)   (8) (0)  (0)  (6)  (9) (0)  (15)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 7 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 8 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 0 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 0 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 6 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 9 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 0 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 15 in

  let n3 = create_node l1 15 l2 in
  let n4 = create_node l3 0 l4 in
  let n5 = create_node l5 15 l6 in
  let n6 = create_node l7 15 l8 in

  let n1 = create_node n3 15 n4 in
  let n2 = create_node n5 30 n6 in

  let root = create_node n1 45 n2 in

  assert (not (is_gdbh root));

(*
                   (root, 36)
                 /            \
          (n1, 10)           (n2, 26)
          /      \           /      \
     (n3, 3)  (n4, 7)  (n5, 11)  (n6, 15)
      /   \     /   \     /   \    /   \
    (1)   (2) (3)  (4)  (5)  (6) (7)  (8)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 1 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 2 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 3 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 4 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 5 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 6 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 7 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 8 in

  let n3 = create_node l1 3 l2 in
  let n4 = create_node l3 7 l4 in
  let n5 = create_node l5 11 l6 in
  let n6 = create_node l7 15 l8 in

  let n1 = create_node n3 10 n4 in
  let n2 = create_node n5 26 n6 in

  let root = create_node n1 36 n2 in

  assert (not (is_gdbh root));

  (*
                   (root, 36)
                 /            \
          (n1, 10)           (n2, 26)
          /      \           /      \
     (n3, 3)  (n4, 7)  (n5, 11)  (n6, 15)
      /   \     /   \     /   \    /   \
    (1)   (2) (3)  (4)  (5)  (6) (7)  (8)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 1 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 2 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 3 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 4 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 5 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 6 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 7 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 8 in

  let n3 = create_node l1 3 l2 in
  let n4 = create_node l3 7 l4 in
  let n5 = create_node l5 11 l6 in
  let n6 = create_node l7 15 l8 in

  let n1 = create_node n3 10 n4 in
  let n2 = create_node n5 26 n6 in

  let root = create_node n1 36 n2 in

  assert (not (is_gdbh root));

  (*
                   (root, 768)
                 /            \
          (n1, 256)           (n2, 512)
          /      \           /      \
     (n3, 128)  (n4, 128) (n5, 256) (n6, 256)
      /   \     /   \     /   \    /   \
    (64)  (64) (64) (64) (128)(128)(128)(128)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 64 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 64 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 64 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 64 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 128 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 128 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 128 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 128 in

  let n3 = create_node l1 128 l2 in
  let n4 = create_node l3 128 l4 in
  let n5 = create_node l5 256 l6 in
  let n6 = create_node l7 256 l8 in

  let n1 = create_node n3 256 n4 in
  let n2 = create_node n5 512 n6 in

  let root = create_node n1 768 n2 in

  assert (is_gdbh root);
;;

let test_btree_equality_ () = 
   let tree = {content = Leaf (EmptyChar, 0); parent = None} in 
   let table = CharaMap.empty in 
   let table = CharaMap.add EmptyChar tree table in

   assert (tree.content = (CharaMap.find EmptyChar table).content);

   let table = insert table (Uchar.of_char 'a') in 
   let table = insert table (Uchar.of_char 'a') in 
   let table = insert table (Uchar.of_char 'b') in 
   let table = insert table (Uchar.of_char 'b') in 
   let table = insert table (Uchar.of_char 'c') in 
   let table = insert table (Uchar.of_char 'd') in 
   let table = insert table (Uchar.of_char 'd') in 
   let table = insert table (Uchar.of_char 'd') in 
   let table = insert table (Uchar.of_char 'a') in 
   let table = insert table (Uchar.of_char 'e') in 
   let table = insert table (Uchar.of_char 'f') in 
   let table = insert table (Uchar.of_char 'e') in 
   let table = insert table (Uchar.of_char 'g') in 

   let rec equal b1 b2 =
      match b1.content, b2.content with 
      | Leaf (c, i), Leaf (c2, i2) -> (c=c2) && (i=i2);
      | Node (e1, i, e2), Node (e11, ii, e22) -> i=ii && equal e1 e11 && equal e2 e22;
      | _ -> false
   in

   let rec find_btree btree e =
      if equal btree e then Some btree
      else
         match btree.content with 
         | Node (e1, _, e2) -> 
            (
               match find_btree e1 e with 
               | None -> find_btree e2 e
               | Some i -> Some i
            )
         | _ -> None
   in

   let rec loop btree = 
      (match btree.content with 
      | Leaf (chara, _) -> 
         (match chara with 
         | EmptyChar -> assert (btree.content = (CharaMap.find EmptyChar table).content)
         | Char c -> assert (btree.content = (CharaMap.find (Char c) table).content)
         );
      | Node (e1, _, e2) ->
         loop e1;
         loop e2;
         (match find_btree tree btree with 
         | None -> assert false;
         | Some b -> assert (b.content == btree.content));
      );
      ()
   in

   loop tree;
   ()
;;


let test_parent_ () =
   (*
                   (root, 768)
                 /            \
          (n1, 256)           (n2, 512)
          /      \           /      \
     (n3, 128)  (n4, 128) (n5, 256) (n6, 256)
      /   \     /   \     /   \    /   \
    (64)  (64) (64) (64) (128)(128)(128)(128)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 64 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 64 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 64 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 64 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 128 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 128 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 128 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 128 in

  let n3 = create_node l1 128 l2 in
  let n4 = create_node l3 128 l4 in
  let n5 = create_node l5 256 l6 in
  let n6 = create_node l7 256 l8 in

  let n1 = create_node n3 256 n4 in
  let n2 = create_node n5 512 n6 in

  let root = create_node n1 768 n2 in

  n1.parent <- Some root;
  n2.parent <- Some root;

  n3.parent <- Some n1;
  n4.parent <- Some n1;
  n5.parent <- Some n2;
  n6.parent <- Some n2;

  l1.parent <- Some n3;
  l2.parent <- Some n3;
  l3.parent <- Some n4;
  l4.parent <- Some n4;
  l5.parent <- Some n5;
  l6.parent <- Some n5;
  l7.parent <- Some n6;
  l8.parent <- Some n6;

  assert (equal_btree (parent l1) n3);
  assert (equal_btree (parent l2) n3);
  assert (equal_btree (parent l3) n4);
  assert (equal_btree (parent l4) n4);
  assert (equal_btree (parent l5) n5);
  assert (equal_btree (parent l6) n5);
  assert (equal_btree (parent l7) n6);
  assert (equal_btree (parent l8) n6);
  
  assert (equal_btree (parent n3) n1);
  assert (equal_btree (parent n4) n1);
  assert (equal_btree (parent n5) n2);
  assert (equal_btree (parent n6) n2);

  assert (equal_btree (parent n1) root);
  assert (equal_btree (parent n2) root);
;;


let test_is_incrementable_ () =
   (*
                   (root, 768)
                 /            \
          (n1, 256)           (n2, 512)
          /      \           /      \
     (n3, 128)  (n4, 128) (n5, 256) (n6, 256)
      /   \     /   \     /   \    /   \
    (64)  (64) (64) (64) (128)(128)(128)(128)
   *)

  let l1 = create_leaf (Char (Uchar.of_char 'a')) 64 in
  let l2 = create_leaf (Char (Uchar.of_char 'b')) 64 in
  let l3 = create_leaf (Char (Uchar.of_char 'c')) 64 in
  let l4 = create_leaf (Char (Uchar.of_char 'd')) 64 in
  let l5 = create_leaf (Char (Uchar.of_char 'e')) 128 in
  let l6 = create_leaf (Char (Uchar.of_char 'f')) 128 in
  let l7 = create_leaf (Char (Uchar.of_char 'g')) 128 in
  let l8 = create_leaf (Char (Uchar.of_char 'h')) 128 in

  let n3 = create_node l1 128 l2 in
  let n4 = create_node l3 128 l4 in
  let n5 = create_node l5 256 l6 in
  let n6 = create_node l7 256 l8 in

  let n1 = create_node n3 256 n4 in
  let n2 = create_node n5 512 n6 in

  let root = create_node n1 768 n2 in 

  let (b, n) = is_incrementable root [n1] in 
  assert b;
  let (b, n) = is_incrementable root [n1; root] in
  assert b;
  let (b, n) = is_incrementable root [l4; n4; n1; root] in
  assert b;
  let (b, n) = is_incrementable root [l6; n5; n2; root] in
  assert (not b);
  let (b, n) = is_incrementable root [n6; n2; root] in 
  assert (not b);
;;

let test_modification_ () =
    (* carambarbcm *)
   let tree = {content = Leaf (EmptyChar, 0); parent = None} in 
   let table = CharaMap.empty in 
   let table = CharaMap.add EmptyChar tree table in

   let table = modification tree table (Uchar.of_char 'c') in (*Printf.printf "c : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'a') in (*Printf.printf "ca : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'r') in (*Printf.printf "car : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'a') in (*Printf.printf "cara : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'm') in (*Printf.printf "caram : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'b') in (*Printf.printf "caramb : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'a') in (*Printf.printf "caramba : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'r') in (*Printf.printf "carambar : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'b') in (*Printf.printf "carambarb : "; print_btree tree; *)
   let table = modification tree table (Uchar.of_char 'c') in (*Printf.printf "carambarbc : "; print_btree tree; *)
   let _ = modification tree table (Uchar.of_char 'm') in Printf.printf "\ncarambarbcm    : "; print_btree tree;
   Printf.printf "Supposed to be : Node(Node(Leaf(b, 2), 4, Leaf(r, 2)), 11, Node(Leaf(a, 3), 7, Node(Node(Leaf(#, 0), 2, Leaf(m, 2)), 4, Leaf(c, 2))))\n\n"
;;

let test_code_ () = 
   let tree = {content = Leaf (EmptyChar, 0); parent = None} in 
   let table = CharaMap.empty in 
   let table = CharaMap.add EmptyChar tree table in

   let table = modification tree table (Uchar.of_char 'c') in 
   let table = modification tree table (Uchar.of_char 'a') in 
   let table = modification tree table (Uchar.of_char 'r') in 
   let table = modification tree table (Uchar.of_char 'a') in 
   let table = modification tree table (Uchar.of_char 'm') in 
   let table = modification tree table (Uchar.of_char 'b') in 
   let table = modification tree table (Uchar.of_char 'a') in 
   let table = modification tree table (Uchar.of_char 'r') in 
   let table = modification tree table (Uchar.of_char 'b') in 
   let table = modification tree table (Uchar.of_char 'c') in 
   let table = modification tree table (Uchar.of_char 'm') in 
   
   assert ((code (Char (Uchar.of_char 'm')) table) = [1; 1; 0; 1]);
   assert ((code (Char (Uchar.of_char 'b')) table) = [0; 0]);
   assert ((code (Char (Uchar.of_char 'r')) table) = [0; 1]);
   assert ((code (Char (Uchar.of_char 'a')) table) = [1; 0]);
   assert ((code (Char (Uchar.of_char 'c')) table) = [1; 1; 1]);
   ()

(* Appeler cette fonction pour executer les tests *)
let test_primitive_ () =
  Printf.printf "\n___ Lancement des tests de test_primitive.ml ___\n";
  try
    test_is_sorted_ ();
    test_is_lte_ ();
    test_is_gte_ ();
    test_finBloc_ ();
    test_switch_ ();
    test_update_weights_ ();
    test_insert_ ();
    test_is_adding_up_ ();
    test_is_gdbh_ ();
    test_parent_ ();
    test_is_incrementable_ ();
    test_btree_equality_ ();
    test_modification_ (); 
    test_code_ ();
    
    Printf.printf "___ Tous les tests de test_primitive.ml sont passés ___\n"
  with e -> Printf.printf "_Un test de test_primitive.ml n'est pas passé : \n%s\n" (Printexc.to_string e);