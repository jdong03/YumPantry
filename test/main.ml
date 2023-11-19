open OUnit2
open Yum
open Quantity
open Ingredient

let pp_string s = "\"" ^ s ^ "\""

let compare_amounts a1 a2 =
  match (a1, a2) with
  | Volume v1, Volume v2 -> Volume.equivalent v1 v2
  | Mass m1, Mass m2 -> Mass.equivalent m2 m2
  | Count c1, Count c2 -> c1 = c2
  | _ -> failwith "amounts are not of the same type!"

let compare_volumes v1 v2 = Volume.equivalent v1 v2

let compare_volume_options v1 v2 =
  match (v1, v2) with Some v1, Some v2 -> Volume.equivalent v1 v2 | _ -> false

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
    (* Simplification.*)
    ( "Simplify 1" >:: fun _ ->
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
        (Mass (Mass.simplify (16.0, Ounce))) );
    ( "Simplify 4" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (3.0, QuarterCup))
        (Volume (Volume.simplify (12.0, Tablespoon))) );
    ( "Simplify 5" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (2.0, Teaspoon))
        (Volume (Volume.simplify (2.0, Teaspoon))) );
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
    ( "Equality 6" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, HalfCup))
        (Volume (0.5, Cup)) );
    (* Scaling *)
    ( "Scaling 1" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (5.0, HalfCup))
        (Volume (Volume.scale 1.0 (5.0, HalfCup))) );
    ( "Scaling 2" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (0.25, Teaspoon))
        (Volume (Volume.scale 0.25 (1.0, Teaspoon))) );
    ( "Scaling 3" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, Quart))
        (Volume (Volume.scale 0.5 (2.0, Quart))) );
    (* Addition *)
    ( "Addition 1" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, Tablespoon))
        (Volume (Volume.add (1.0, Teaspoon) (2.0, Teaspoon))) );
    ( "Addition 2" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (0.0, Teaspoon))
        (Volume (Volume.add (0.0, Gallon) (0.0, Pint))) );
    ( "Addition 3" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (1.0, Gallon))
        (Volume (Volume.add (3.0, Quart) (2.0, Pint))) );
    (* Subtraction *)
    ( "Subtraction 1" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (0.0, Tablespoon))
        (Volume (Volume.subtract (1.0, Gallon) (1.0, Gallon))) );
    ( "Subtraction 2" >:: fun _ ->
      assert_equal ~cmp:compare_amounts
        (Volume (3.0, HalfCup))
        (Volume (Volume.subtract (2.0, Cup) (1.0, HalfCup))) );
    (* Less than *)
    ( "Less than 1" >:: fun _ ->
      assert_equal true (Volume.less_than (1.0, Quart) (1.0, Gallon)) );
    ( "Less than 2" >:: fun _ ->
      assert_equal false (Volume.less_than (5.0, Quart) (1.0, Gallon)) );
    (* TODO: greater than *)
    (* Of string *)
    ( "Test 1" >:: fun _ ->
      assert_equal ~cmp:compare_volume_options
        (Volume.of_string "3.0 Quart")
        (Some (3.0, Quart)) );
    ( "Test 2" >:: fun _ ->
      assert_equal ~cmp:compare_volume_options
        (Volume.of_string "30.0 Teaspoon")
        (Some (30.0, Teaspoon)) );
    ( "Test 3" >:: fun _ ->
      assert_equal ~cmp:compare_volume_options
        (Volume.of_string "0.0 Pint")
        (Some (0.0, Pint)) );
  ]

let apple = { name = "Apple"; measurement_type = MCount }
let beef = { name = "Beef"; measurement_type = MMass }

let pantry_tests =
  [
    (*Empty pantry*)
    ("Empty Test" >:: fun _ -> assert_equal "" (Pantry.display Pantry.empty));
    ( "Empty Remove" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry -> Pantry.remove pantry apple (Count 1.0))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Empty Remove two ingredients" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry -> Pantry.remove pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.remove pantry beef (Mass (8.0, Ounce)))
        |> Pantry.display)
        ~printer:pp_string );
    (*Add tests*)
    ( "Add one ingredient" >:: fun _ ->
      assert_equal "\n1. of Apple"
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add two ingredients" >:: fun _ ->
      assert_equal "\n1. of Apple\n8. ounces of Beef"
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.add pantry beef (Mass (8.0, Ounce)))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add two ingredients of same type" >:: fun _ ->
      assert_equal "\n2. of Apple"
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add two ingredients of same type with different amounts" >:: fun _ ->
      assert_equal "\n3. of Apple"
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.add pantry apple (Count 2.0))
        |> Pantry.display)
        ~printer:pp_string );
    (*Remove tests*)
    ( "Remove one ingredient" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.remove pantry apple (Count 1.0))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Remove two ingredients" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.add pantry beef (Mass (8.0, Ounce)))
        |> (fun pantry -> Pantry.remove pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.remove pantry beef (Mass (8.0, Ounce)))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Remove more ingredients than pantry has" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.remove pantry apple (Count 2.0))
        |> Pantry.display)
        ~printer:pp_string );
    (*Reset tests*)
    ( "Reset empty pantry" >:: fun _ ->
      assert_equal ""
        (Pantry.empty |> Pantry.reset |> Pantry.display)
        ~printer:pp_string );
    ( "Reset pantry with one ingredient" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> Pantry.reset |> Pantry.display)
        ~printer:pp_string );
    ( "Reset pantry with two ingredients" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry -> Pantry.add pantry apple (Count 1.0))
        |> (fun pantry -> Pantry.add pantry beef (Mass (8.0, Ounce)))
        |> Pantry.reset |> Pantry.display)
        ~printer:pp_string );
  ]

let suite = "test suite" >::: List.flatten [ quantity_tests; pantry_tests ]
let () = run_test_tt_main suite
