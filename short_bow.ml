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

  (** The outer radius with which the bow can shoot. *)
  let bow_or = 7

  (** The inner radius with which the bow cannot shoot. *)
  let bow_ir = 2

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

  (** [in_radius coord inr outr] is true if the coordinate is within [or] units of 
      the origin and greater than [ir] units of the origin. *)
  let in_radius coord inr outr =
    let b =
      distance_sq coord (0,0) < (square outr)
      && distance_sq coord (0, 0) > (square inr)
    in print_string (string_of_bool b); b
  (** [filter_in_radius lst] is the list [lst] filtered to only include 
      coordinates within the inner and outer bow radius'. *)
  let filter_in_radius lst = 
    List.filter (fun c -> in_radius c bow_ir bow_or) lst

  (** The coordinates matching the top semi-circle of the attack range. *)
  let top_semi_circle = 
    let rect = make_rect_coords (-bow_or - 1) 1 (2 * bow_or) 
        (bow_or + 1) in 
    filter_in_radius rect

  (** The coordinates matching the left semi-circle of the attack range. *)
  let left_semi_circle = 
    let rect = make_rect_coords (-bow_or - 1) (-bow_or - 1) 
        (bow_or + 1) (2 * bow_or) in 
    filter_in_radius rect

  (** The coordinates matching the right semi-circle of the attack range. *)
  let right_semi_circle = 
    let rect = make_rect_coords 1 (-bow_or + 1) (bow_or + 1) 
        (2 * bow_or + 2) in 
    filter_in_radius rect

  (** The coordinates matching the bottom semi-circle of the attack range. *)
  let bottom_semi_circle = 
    let rect = make_rect_coords (-bow_or - 1) (-bow_or - 1) 
        (2 * bow_or + 2) (bow_or + 1) in 
    filter_in_radius rect

  let attack w dir = 
    match dir with
    | Action.Up ->    List.map (fun (x, y) -> (x, y, w.damage)) top_semi_circle
    | Action.Down ->  List.map (fun (x, y) -> (x, y, w.damage)) 
                        right_semi_circle
    | Action.Left ->  List.map (fun (x, y) -> (x, y, w.damage)) left_semi_circle
    | Action.Right -> List.map (fun (x, y) -> (x, y, w.damage)) 
                        bottom_semi_circle

end
