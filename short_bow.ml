open Weapon
open Math

module Short_Bow : Weapon_Type = struct

  let create_weapon level = {
    name = "Short Bow";
    position = Some (-1, -1);
    w_type = ShortBow;
    damage = level;
    atk_type = Ranged;
    level = level;
  }

  (** The radius with which the bow can shoot. *)
  let bow_radius = 7

  (** [make_rect_coords leftX bottomY w h] lists the coordinates
      [(leftX, bottomY)] through [(leftX + w - 1, bottomY + h - 1)]. *)
  let make_rect_coords leftX bottomY w h =
    let lst = ref [] in
    for x = leftX to leftX + w - 1 do
      for y = bottomY to bottomY + h - 1 do
        lst := (x, y) :: !lst
      done
    done;
    !lst

  (** [in_radius coord r] is true if the coordinate is within [r] units of the
      origin. *)
  let in_radius coord r =
    distance_sq coord (0,0) < (square bow_radius)

  (** The coordinates matching the top semi-circle of the attack range. *)
  let top_semi_circle = 
    let rect = make_rect_coords (-bow_radius - 1) 1 (2 * bow_radius) 
        (bow_radius + 1) in 
    List.filter (fun c -> in_radius c bow_radius) rect

  (** The coordinates matching the left semi-circle of the attack range. *)
  let left_semi_circle = 
    let rect = make_rect_coords (-bow_radius - 1) (-bow_radius - 1) 
        (bow_radius + 1) (2 *bow_radius) in 
    List.filter (fun c -> in_radius c bow_radius) rect

  (** The coordinates matching the right semi-circle of the attack range. *)
  let right_semi_circle = 
    let rect = make_rect_coords 1 (-bow_radius + 1) (bow_radius + 1) 
        (2 * bow_radius + 2) in 
    List.filter (fun c -> in_radius c bow_radius) rect

  (** The coordinates matching the bottom semi-circle of the attack range. *)
  let bottom_semi_circle = 
    let rect = make_rect_coords (-bow_radius - 1) (-bow_radius - 1) 
        (2 * bow_radius + 2) (bow_radius + 1) in 
    List.filter (fun c -> in_radius c bow_radius) rect

  let attack w dir = 
    match dir with
    | Action.Up ->    List.map (fun (x, y) -> (x, y, w.damage)) top_semi_circle
    | Action.Down ->  List.map (fun (x, y) -> (x, y, w.damage)) 
                        right_semi_circle
    | Action.Left ->  List.map (fun (x, y) -> (x, y, w.damage)) left_semi_circle
    | Action.Right -> List.map (fun (x, y) -> (x, y, w.damage)) 
                        bottom_semi_circle

end
