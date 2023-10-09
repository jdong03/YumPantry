(** Manage quantities of ingredients. *)

(** Represent a measured quantity*)
module type Measurement = sig
  type units
  (** The unit representation of the measurement, e.g. teaspoons, pounds, etc. *)

  type measure
  (** An actual quantity of the measurement *)

  val simplify : measure -> measure
  (** Reduces a measurement to a simpler representation if possible. For example, 
        [reduce (4.0, Cup)] evaluates to [(1.0, Quart)].*)

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

type volume_units =
  | Teaspoon
  | Tablespoon
  | QuarterCup
  | HalfCup
  | Cup
  | Pint
  | Quart
  | Gallon

(** Simple implementation of volume measurements *)
module Volume :
  Measurement
    with type units = volume_units
     and type measure = float * volume_units

type mass_units = Ounce | Pound

(** Simple implementation of mass measurements *)
module Mass :
  Measurement with type units = mass_units and type measure = float * mass_units

type amount = Volume of Volume.measure | Weight of Mass.measure | Count of int
