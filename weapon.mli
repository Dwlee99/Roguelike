(** Represents generic information for each Weapon that will be in the game. *)

(** Represents how damage is computed. *)
type damage = int

(** An [attack_type] is [Melee] or [Ranged]. *)
type attack_type = 
  | Melee
  | Ranged

(** A representation of weapons in the game. *)
type weapon = {
  name : string;
  position : (int * int) option;
  w_type : Board.weapon_type;
  damage : damage;
  atk_type : attack_type;
  level : int;
}

(** Modules of this type define a new weapon to be added to the game. *)
module type Weapon_Type = sig

  (** [create_weapon level] is a new weapon of level [level]. *)
  val create_weapon : int -> weapon

  (** [attack w dir] is a list of coordinates affected by an attack [w] in 
      direction [dir] and the damage done to each of the tiles at those 
      coordinates. *)
  val attack : weapon -> Action.direction -> (int * int * damage) list

end

(** [get_type w] is w.w_type. *)
val get_type : weapon -> Board.weapon_type