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

let test_is_lte_ () = 
  let open Primitive in
  
  let emptyLeaf = Leaf (EmptyChar, 0) in 
  assert (is_lte emptyLeaf 0);
  assert (is_lte emptyLeaf 100);
  assert (not (is_lte emptyLeaf (-100)));

  let negativeLeaf = Leaf (EmptyChar, -1000) in 
  assert (is_lte negativeLeaf 10);
  assert (is_lte negativeLeaf (-1000));
  assert (not (is_lte negativeLeaf (-4125)));

  let positiveLeaf = Leaf (EmptyChar, 1692) in 
  assert (is_lte positiveLeaf 1692);
  assert (not (is_lte positiveLeaf (-1340)));
  assert (is_lte positiveLeaf 11564);

  let tree1 = Node (emptyLeaf, 0, emptyLeaf) in 
  assert (is_lte tree1 0);
  assert (is_lte tree1 100);
  assert (not (is_lte tree1 (-100)));

  let tree1_expended = 
    Node (
      Node (
        emptyLeaf,0, Node(emptyLeaf,0,emptyLeaf)
      ),
      0,
      Node (
        Node(
          Node(
            Node(
              Node(emptyLeaf,0, emptyLeaf),0, emptyLeaf
            ),
            0,
            Node(emptyLeaf,0, emptyLeaf)
          ),
          0,
          emptyLeaf
        ),
        0,
        Node(
          emptyLeaf,0, emptyLeaf
        )
     )
    )
  in 
  assert (is_lte tree1_expended 0);
  assert (is_lte tree1_expended 100);
  assert (not (is_lte tree1_expended (-100)));

  let tree2 =
    Node(
      Node(
        Leaf(EmptyChar, 100),
        4,
        Leaf(EmptyChar, 130)
      ),
      67,
      Node(
        Node(
          Leaf(EmptyChar, 180),
          148,
          Node(
            Leaf(EmptyChar, 170),
            138,
            Node(
              Node(
                Leaf(EmptyChar, 128),
                68,
                Leaf(EmptyChar, 201)
              ),
              200,
              emptyLeaf
            )
          )
        ),
        49,
        Leaf(EmptyChar, 120)
      )
    )
  in
  assert (is_lte tree2 201);
  assert (is_lte tree2 202);
  assert (not (is_lte tree2 199));
  assert (not (is_lte tree2 0));

  let tree1_expended = 
    Node (
      Node (
        emptyLeaf,0, Node(emptyLeaf,0,emptyLeaf)
      ),
      0,
      Node (
        Node(
          Node(
            Node(
              Node(emptyLeaf,0, emptyLeaf),0, emptyLeaf
            ),
            0,
            Node(emptyLeaf,0, emptyLeaf)
          ),
          0,
          emptyLeaf
        ),
        0,
        Node(
          emptyLeaf,0, emptyLeaf
        )
     )
    )
  in 
  assert (is_lte tree1_expended 0);
  assert (is_lte tree1_expended 100);
  assert (not (is_lte tree1_expended (-100)));

  let tree2 =
    Node(
      Node(
        Leaf(EmptyChar, 100),
        4,
        Leaf(EmptyChar, 130)
      ),
      67,
      Node(
        Node(
          Leaf(EmptyChar, 180),
          148,
          Node(
            Leaf(EmptyChar, 170),
            138,
            Node(
              emptyLeaf,
              201,
              Node(
                Leaf(EmptyChar, 128),
                68,
                Leaf(EmptyChar, 200)
              )
            )
          )
        ),
        49,
        Leaf(EmptyChar, 120)
      )
    )
  in
  assert (is_lte tree2 201);
  assert (is_lte tree2 202);
  assert (not (is_lte tree2 199));
  assert (not (is_lte tree2 0));
;;

let test_is_gte_ () =
  failwith "todo"
;;

(* Appeler cette fonction pour executer les tests *)
let test_primitive_ () =
  Printf.printf "\n___ Lancement des tests de test_primitive.ml ___\n";
  try
    test_is_sorted_ ();
    test_is_lte_ ();
    test_is_gte_ ();
    Printf.printf "___ Tous les tests de test_primitive.ml sont passés ___\n"
  with e -> Printf.printf "_Un test de test_primitive.ml n'est pas passé : \n%s\n" (Printexc.to_string e);