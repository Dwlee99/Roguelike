(** Represents generic information for each Weapon that will be in the game. *)

(** Represents how damage is computed. *)
type damage = int

(** A [weapon_type] is one of the weapons added to the game. *)
type weapon_type = 
  | Short_Sword
  | Battleaxe
  | Short_Bow

(** An [attack_type] is [Melee] or [Ranged]. *)
type attack_type = 
  | Melee
  | Ranged

(** A representation of [weapons] in the game. *)
type weapon = {
  name : string;
  w_type : weapon_type;
  damage : damage;
  atk_type : attack_type;
}

(** A module for defining a new weapon to be added to the game. *)
module type Weapon_Type = sig

  val create_weapon : int -> weapon

  val attack : weapon -> Action.direction -> (int * int * damage) list

end

(** [get_type w] is w.w_type. *)
val get_type : weapon -> weapon_type