open Weapon

module Battleaxe : Weapon_Type = struct

  let create_weapon level = {
    name = "Battleaxe";
    w_type = Battleaxe;
    damage = level;
    atk_type = Melee;
  }

  let attack w dir = 
    match dir with
    | Action.Up ->    List.init 6 (fun x ->
        ((x mod 3) - 1, (x / 3) + 1, if (x mod 3) - 1 = 0 then 2 * w.damage else 0))
    | Action.Down ->  List.init 6 (fun x ->
        ((x mod 3) - 1, (-x / 3) - 1, if (x mod 3) - 1 = 0 then 2 * w.damage else 0))
    | Action.Left ->  List.init 6 (fun x ->
        ((-x / 3) - 1, (x mod 3) - 1, if (x mod 3) - 1 = 0 then 2 * w.damage else 0))
    | Action.Right -> List.init 6 (fun x ->
        ((x / 3) + 1, (x mod 3) - 1, if (x mod 3) - 1 = 0 then 2 * w.damage else 0))

end