open OUnit2
open Yum
open Quantity

let compare_amounts a1 a2 = a1 = a2

let quantity_tests =
  [
    ( "Convert 1" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, Teaspoon))
        (Volume (Volume.convert (1.0, Teaspoon) Teaspoon)) );
    ( "Convert 2" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, Tablespoon))
        (Volume (Volume.convert (3.0, Teaspoon) Tablespoon)) );
    ( "Convert 3" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, Gallon))
        (Volume (Volume.convert (16.0, Cup) Gallon)) );
    ( "Simplify 1" >:: fun _ ->
      assert_equal ~cmp:compare_amounts (1.0, Tablespoon)
        (Volume.simplify (3.0, Teaspoon)) );
    ( "Simplify 2" >:: fun _ ->
      assert_equal ~cmp:compare_amounts (1.0, Gallon)
        (Volume.simplify (16.0, Cup)) );
    ( "Simplify 3" >:: fun _ ->
      assert_equal ~cmp:compare_amounts (1.0, Pound)
        (Mass.simplify (16.0, Ounce)) );
    ( "Equality operator 1" >:: fun _ ->
      assert_equal true (Volume (1.0, Teaspoon) = Volume (1.0, Teaspoon)) );
    ( "Equality operator 2" >:: fun _ ->
      assert_equal true (Volume (5.5, Gallon) = Volume (5.5, Gallon)) );
    ( "Equality operator 3" >:: fun _ ->
      assert_equal true (Volume (3.0, Teaspoon) = Volume (1.0, Tablespoon)) );
  ]

let suite = "test suite for A2" >::: List.flatten [ quantity_tests ]
let () = run_test_tt_main suite
