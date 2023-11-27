open Ingredient

val calc_score : string -> string -> int
(* [calc_score s1 s2] calculates the Levenshtein distance of s1 and s2. *)

val match_string : string -> Ingredient.t option
(* [match_string s] "autocorrects" s by matching it to an ingredient in 
   Ingredient.al_ingredients. Returns the ingredient that has the lowest 
   Levenshtein distance with s if the distance is less than 3. Returns None otherwise. *)
