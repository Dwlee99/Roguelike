open Weapon

module Short_Sword : Weapon_Type = struct

  let create_weapon level = {
    name = "Short Sword";
    position = Some (-1, -1);
    w_type = ShortSword;
    damage = level;
    atk_type = Melee;
    level = level;
  }

  let attack w dir = 
    match dir with
    | Action.Up ->    [(0, 1, 3 * w.damage)]
    | Action.Down ->  [(0, -1, 3 * w.damage)]
    | Action.Left ->  [(-1, 0, 3 * w.damage)]
    | Action.Right -> [(1, 0, 3 * w.damage)]

end
