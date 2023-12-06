(** Module for managing quantities of ingredients in various units.
    
    This module provides types and functions to represent and manipulate
    quantities in units such as mass, volume, and count. It supports basic
    arithmetic operations and conversions between different units.

    Example Usage:
    - Representing three bell peppers: `Count 3.0`
    - Five tablespoons of milk: `Volume (5.0, Tablespoon)`
    - Two pounds of chicken breast: `Mass (2.0, Pound)`
*)

type units
(** The type representing various units of measurement. *)

type t
(** The type representing a quantity, paired with its unit of measurement. *)

val simplify : t -> t
(** [simplify quantity] reduces a quantity to its simplest form.
    - Example: [simplify (4.0, Cup)] evaluates to [(1.0, Quart)]. *)

val add : t -> t -> t option
(** [add q1 q2] adds two quantities of the same unit. 
    - Returns [None] if units are incompatible.
    - Example: [add (1.0, Cup) (2.0, Cup)] evaluates to [Some (3.0, Cup)]. *)

val subtract : t -> t -> t option
(** [subtract q1 q2] subtracts the second quantity from the first.
    - Requires: The units of [q1] and [q2] are the same and [q1 >= q2].
    - Returns [None] if conditions are not met.
    - Example: [subtract (2.0, Cup) (1.0, Cup)] evaluates to [Some (1.0, Cup)]. *)

val scale : float -> t -> t
(** [scale factor quantity] scales the quantity by the given factor.
    - Example: [scale 2.0 (1.0, Cup)] evaluates to [(2.0, Cup)]. *)

val greater_than : t -> t -> bool option
(** [greater_than q1 q2] checks if [q1] is greater than [q2].
    - Returns [None] if units are incompatible.
    - Example: [greater_than (2.0, Cup) (1.0, Cup)] evaluates to [Some true]. *)

val less_than : t -> t -> bool option
(** [less_than q1 q2] checks if [q1] is less than [q2].
    - Returns [None] if units are incompatible.
    - Example: [less_than (1.0, Cup) (2.0, Cup)] evaluates to [Some true]. *)

val equivalent : t -> t -> bool option
(** [equivalent q1 q2] checks if [q1] is equivalent to [q2].
    - Returns [None] if units are incompatible.
    - Example: [equivalent (48.0, Teaspoon) (1.0, Cup)] evaluates to [Some true]. *)

val of_string : string -> t option
(** [of_string s] attempts to parse a string into a quantity.
    - The string must be in the format "float units" (e.g., "3.0 Teaspoon").
    - Returns [None] if parsing fails.
    - Example: [of_string "3.0 Teaspoon"] evaluates to [Some (3.0, Teaspoon)]. *)

val to_string : t -> string
(** [to_string quantity] converts a quantity to its string representation.
    - Example: [to_string (1.0, Cup)] evaluates to ["1.0 Cup"]. *)

val same_type_of_units : units -> units -> bool
(** [same_type_of_units u1 u2] checks if [u1] and [u2] are of the same type.
    - Example: [same_type_of_units Cup Quart] evaluates to [true]. *)

val units_of_quantity : t -> units
(** [units_of_quantity quantity] returns the unit of the given quantity.
    - Example: [units_of_quantity (1.0, Cup)] evaluates to [Cup]. *)

val units_of_string : string -> units option
(** [units_of_string s] attempts to parse a string into a unit.
    - Returns [None] if parsing fails.
    - Example: [units_of_string "Cup"] evaluates to [Some Cup]. *)

val measurement_type : units -> string
(** [measurement_type unit] returns the measurement type of the given unit as a string.
    - Example: [measurement_type Cup] evaluates to ["Volume"]. *)

val is_neg : t -> bool
(** [is_neg quantity] checks if the quantity is negative.
    - Example: [is_neg (-1.0, Cup)] evaluates to [true]. *)

val is_zero : t -> bool
(** [is_zero quantity] checks if the quantity is zero.
    - Example: [is_zero (0.0, Cup)] evaluates to [true]. *)
