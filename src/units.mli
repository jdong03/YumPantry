(** Common cooking units *)

type volume_unit
(** The unit representation of the volume, e.g. teaspons, cups, etc. *)

type volume
(** An actual quantity of volume *)

val reduce : volume -> volume
(** Reduces a volume to a simpler representation if possible. For example, 
    [reduce (4.0, Cup)] evaluates to [(1.0, Quart)].*)

val convert : volume -> volume_unit -> volume
(** Convert a volume in one unit to a volume in another unit *)

val ( + ) : volume -> volume -> volume
(** [vol1 + vol2 ] is the addition of the volumes in the largest possible 
    unit representation. Returned in simplest form *)

val ( - ) : volume -> volume -> volume option
(** [vol1 - vol2 ] is the subtraction of the volumes in the largest possible 
    unit representation. Requires [vol1 >= vol2]. Returned in simplest form *)
