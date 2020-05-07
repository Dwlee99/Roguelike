type damage = int

type weapon_type = 
  | Short_Sword
  | Battleaxe
  | Short_Bow

type attack_type = 
  | Melee
  | Ranged

type weapon = {
  name : string;
  w_type : weapon_type;
  damage : damage;
  atk_type : attack_type;
}

module type Weapon_Type = sig

  val create_weapon : int -> weapon

  val attack : weapon -> Action.direction -> (int * int * damage) list

end