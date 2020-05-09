(** Represents armor in the game. *)

(** The way protection values are represented. *)
type protection = int

(** Represents the data a piece of armor possesses. *)
type armor = {
  name : string;
  protection : protection;
  level : int;
}

(** [create_armor level] is armor with the level [level]. *)
val create_armor: int -> armor