type damage = int

type weapon_type = 
  | Short_Sword
  | Battleax

type weapon = {
  name : string;
  w_type : weapon_type;
  damage : damage;
}

module type Weapon_Type = sig

  val attack : weapon -> Action.direction -> (int * int * damage) list

end