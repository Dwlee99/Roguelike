open Weapon

module Short_Sword : Weapon_Type = struct

  let create_weapon level = {
    name = "Short Sword";
    w_type = Short_Sword;
    damage = level;
  }

  let attack w dir = 
    match dir with
    | Action.Up ->    [(0, 1, w.damage)]
    | Action.Down ->  [(0, -1, w.damage)]
    | Action.Left ->  [(-1, 0, w.damage)]
    | Action.Right -> [(1, 0, w.damage)]

end
