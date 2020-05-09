open Weapon

module Short_Bow : Weapon_Type = struct

  let create_weapon level = {
    name = "Short Bow";
    position = Some (-1, -1);
    w_type = ShortBow;
    damage = level;
    atk_type = Ranged;
    level = level;
  }

  let attack w dir = 
    match dir with
    | Action.Up ->    List.init 5 (fun x -> (0, x, w.damage))
    | Action.Down ->  List.init 5 (fun x -> (0, -x, w.damage))
    | Action.Left ->  List.init 5 (fun x -> (-x, 0, w.damage))
    | Action.Right -> List.init 5 (fun x -> (x, 0, w.damage))

end
