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
          let remaining = fridge - recipe_requirement;;
        ]
      * [remaining] should evaluate to [Volume (3.0, HalfCup)]
*)

(** Available volume units *)
type volume_units =
  | Teaspoon
  | Tablespoon
  | QuarterCup
  | HalfCup
  | Cup
  | Pint
  | Quart
  | Gallon

(** Available mass units *)
type mass_units = Ounce | Pound

(** Represent a measured quantity*)
module type Measurement = sig
  type units
  (** The unit representation of the measurement, e.g. teaspoons, pounds, etc. *)

  type measure
  (** An actual quantity of the measurement *)

  val simplify : measure -> measure
  (** Reduces a measurement to a simpler representation if possible. For example, 
        [simplify (4.0, Cup)] evaluates to [(1.0, Quart)].*)

  val convert : measure -> units -> measure
  (** Convert a measurement in one unit to a volume in another unit *)

  val ( + ) : measure -> measure -> measure
  (** [m1 + m2 ] is the addition of the measurements in the largest possible 
        unit representation. Returned in simplest form *)

  val ( - ) : measure -> measure -> measure
  (** [m1 - m2 ] is the subtraction of the measurements in the largest possible 
        unit representation. Requires [m1 >= m2]. Returned in simplest form *)

  val ( > ) : measure -> measure -> bool
  (** [m1 > m2] is [true] if m1 is a greater quantity than m2. *)

  val ( < ) : measure -> measure -> bool
  (** [m1 < vol2] is [true] if m1 is a lesser quantity than m2. *)

  val ( == ) : measure -> measure -> bool
  (** [m1 == m2] is [true] if m1 is the same quantity as m2. *)
end

(** Volume measurements *)
module Volume :
  Measurement
    with type units = volume_units
     and type measure = float * volume_units

(** Mass measurements *)
module Mass :
  Measurement with type units = mass_units and type measure = float * mass_units

type amount =
  | Volume of Volume.measure
  | Weight of Mass.measure
  | Count of float
