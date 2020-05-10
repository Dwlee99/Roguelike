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
    | Action.Up ->    List.init 3 (fun x -> (x - 1, 1, 3 * w.damage))
    | Action.Down ->  List.init 3 (fun x -> (x - 1, -1, 3 * w.damage))
    | Action.Left ->  List.init 3 (fun x -> (-1, x - 1, 3 * w.damage))
    | Action.Right -> List.init 3 (fun x -> (1, x - 1, 3 * w.damage))

end
