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
  failwith "todo"


(* Appeler cette fonction pour executer les tests *)
let test_primitive_ () =
  Printf.printf "\n___ Lancement des tests de test_primitive.ml ___\n";
  try
    test_is_sorted_ ();
    test_is_lte_ ();
    test_is_gte_ ();
    Printf.printf "___ Tous les tests de test_primitive.ml sont passés ___\n"
  with e -> Printf.printf "_Un test de test_primitive.ml n'est pas passé : \n%s\n" (Printexc.to_string e);