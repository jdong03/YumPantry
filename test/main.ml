open OUnit2
open Yum
open Quantity

let compare_amounts a1 a2 = a1 = a2

let quantity_tests =
  [
    ( "Simplify 1" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (3.0, Teaspoon))
        (Volume (1.0, Tablespoon)) );
    ( "Simplify 2" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (16.0, Cup))
        (Volume (1.0, Gallon)) );
    ( "Simplify 3" >:: fun _ ->
      assert_equal ~cmp:compare_amounts (Mass (16.0, Ounce)) (Mass (1.0, Pound))
    );
    ( "Equality operator 1" >:: fun _ ->
      assert_equal true (Volume (1.0, Teaspoon) = Volume (1.0, Teaspoon)) );
    ( "Equality operator 2" >:: fun _ ->
      assert_equal true (Volume (5.5, Gallon) = Volume (5.5, Gallon)) );
    ( "Equality operator 3" >:: fun _ ->
      assert_equal true (Volume (3.0, Teaspoon) = Volume (1.0, Tablespoon)) );
  ]

let suite = "test suite for A2" >::: List.flatten [ quantity_tests ]
let () = run_test_tt_main suite
