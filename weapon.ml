
type damage = int

type attack_type = 
  | Melee
  | Ranged

type weapon = {
  name : string;
  position : (int * int) option;
  w_type : Board.weapon_type;
  damage : damage;
  atk_type : attack_type;
  level : int;
}

module type Weapon_Type = sig

  (** [create_weapon level] is a new weapon of level [level]. *)
  val create_weapon : int -> weapon

  (** [attack w dir] is a list of coordinates affected by an attack [w] in 
      direction [dir] and the damage done to each of the tiles at those 
      coordinates. *)
  val attack : weapon -> Action.direction -> (int * int * damage) list
end

let get_type w = w.w_type