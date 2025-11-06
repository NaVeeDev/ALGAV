let test_ () =
  Test_utils.test_utils_ ();
  Test_primitive.test_primitive_ ();
  Test_compression.test_compression_ ()
;;

test_ ()