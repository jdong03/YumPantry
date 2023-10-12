open OUnit2
open Yum
open Quantity

let compare_amounts a1 a2 =
  match (a1, a2) with
  | Volume v1, Volume v2 -> Volume.equivalent v1 v2
  | Mass m1, Mass m2 -> Mass.equivalent m2 m2
  | Count c1, Count c2 -> c1 = c2
  | _ -> failwith "amounts are not of the same type!"

let quantity_tests =
  [
    (* Conversion *)
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
    (* Simplification. FIXME: broken *)
    (* ( "Simplify 1" >:: fun _ ->
         assert_equal ~cmp:compare_amounts
           (Volume (1.0, Tablespoon))
           (Volume (Volume.simplify (3.0, Teaspoon))) );
       ( "Simplify 2" >:: fun _ ->
         assert_equal ~cmp:compare_amounts
           (Volume (1.0, Gallon))
           (Volume (Volume.simplify (16.0, Cup))) );
       ( "Simplify 3" >:: fun _ ->
         assert_equal ~cmp:compare_amounts
           (Mass (1.0, Pound))
           (Mass (Mass.simplify (16.0, Ounce))) ); *)
    (* Equality *)
    ( "Equality 1" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, Teaspoon))
        (Volume (1.0, Teaspoon)) );
    ( "Equality 2" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (5.5, Gallon))
        (Volume (5.5, Gallon)) );
    ( "Equality 3" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (3.0, Teaspoon))
        (Volume (1.0, Tablespoon)) );
    ( "Equality 4" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (3.0, Teaspoon))
        (Volume (1.0, Tablespoon)) );
    ( "Equality 4" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (2.5, Cup))
        (Volume (5.0, HalfCup)) );
    ( "Equality 5" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (0.5, Pint))
        (Volume (16.0, Tablespoon)) );
  ]

let suite = "test suite for A2" >::: List.flatten [ quantity_tests ]
let () = run_test_tt_main suite
