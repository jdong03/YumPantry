(** Common cooking units *)

module type Measurement = sig
  type units
  (** The unit representation of the measurement, e.g. teaspons, pounds, etc. *)

  type measure
  (** An actual quantity of the measuremnt *)

  val simplify : measure -> measure
  (** Reduces a measurement to a simpler representation if possible. For example, 
        [reduce (4.0, Cup)] evaluates to [(1.0, Quart)].*)

  val convert : measure -> units -> measure
  (** Convert a measurement in one unit to a volume in another unit *)

  val ( + ) : measure -> measure -> measure
  (** [m1 + m2 ] is the addition of the measurments in the largest possible 
        unit representation. Returned in simplest form *)

  val ( - ) : measure -> measure -> measure option
  (** [m1 - m2 ] is the subtraction of the measurements in the largest possible 
        unit representation. Requires [m1 >= m2]. Returned in simplest form *)

  val ( > ) : measure -> measure -> bool
  (** [m1 > m2] is [true] if m1 is a greater quantity than m2. *)

  val ( < ) : measure -> measure -> bool
  (** [m1 < vol2] is [true] if m1 is a lesser quantity than m2. *)

  val ( == ) : measure -> measure -> bool
  (** [m1 == m2] is [true] if m1 is the same quantity as m2. *)
end

module Volume : Measurement
(** Simple implementation of volume measurements *)

module Weight : Measurement
(** Simple implementation of weight measurements *)

type amount =
  | Volume of Volume.measure
  | Weight of Weight.measure
  | Count of int
