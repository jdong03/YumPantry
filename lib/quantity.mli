(** Manage quantities of ingredients.
    
    Usage:
    The primary use of this compilation unit is the [type amount] defined
    at the bottom of this file.
      * For example, say you need to represent three bell peppers:
        [Count 3.0] 
      * Say you need five tablespoons of milk:
        [Volume (5.0, Tablespoon)]
      * Or, say you need two pounds of chicken breast:
        [Mass (2.0, Pound)]

    The [Measurement] module provides arithmetic for complicated cooking units.
      * For example, [
          let recipe_requirement = Volume (1.0, HalfCup);;
          let fridge = Volume (2.0, Cup);;
          let remaining = Volume.subtract fridge recipe_requirement;;
        ]
      * [remaining] should evaluate to [Volume (3.0, HalfCup)]

    NOTE: many of these functions return option. None is returned if the
          operation operates on invalid types, e.g. attempting to add a mass and
          a volume
*)

type units
(** The various units in which a quantity may be measured*)

type t
(** Quantity type *)

val simplify : t -> t
(** Reduces a quantity to a simpler representation if possible. For example, 
      [simplify (4.0, Cup)] evaluates to [(1.0, Quart)].*)

val add : t -> t -> t option
(** [add q1 q2] is the addition of the quantities [q1 + q2].*)

val subtract : t -> t -> t option
(** [subtract q1 q2 ] is the subtraction of the measurements [q1 - q2]. 
    Requires [m1 >= m2].  *)

val scale : float -> t -> t
(** [scale f m] is m scaled by f.*)

val greater_than : t -> t -> bool option
(** [greater_than q1 q2] is [true] if q1 is a greater quantity than q2. *)

val less_than : t -> t -> bool option
(** [less_than q1 q2] is [true] if q1 is a lesser quantity than q2. *)

val equivalent : t -> t -> bool option
(** [equivalent q1 q2] is [true] if q1 is the same quantity as q2. *)

val of_string : string -> t option
(** Attempts to convert a string to a measure. String must be formatted as
    "float units", e.g., "3.0 Teaspoon".*)

val to_string : t -> string
(** Converts a quantity to a string.*)

val same_type_of_units : units -> units -> bool

val units_of_quantity : t -> units

val units_of_string : string -> units option

val units_of_quantity : t -> units

val measurement_type : units -> string