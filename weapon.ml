
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

  val create_weapon : int -> weapon

  val attack : weapon -> Action.direction -> (int * int * damage) list
end

let get_type w = w.w_type