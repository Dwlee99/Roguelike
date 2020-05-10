open Weapon
open Math
open Float

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
  let bow_or = 6

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

  (** [angle_btwn angle lt ht] is true if [angle] lies between the smallest path
      from lt to ht. *)
  let angle_btwn angle lt ht = 
    let n_end = ht -. lt in
    let n_mid = angle -. lt in
    let n_end = if n_end < 0.0 then n_end +. (2.0 *. pi) else n_end in
    let n_mid = if n_mid < 0.0 then n_mid +. (2.0 *. pi) else n_mid in
    n_mid < n_end

  (** [in_radius coord inr outr lowthet highthet] is true if the coordinate is 
      within [or] units of the origin and greater than [ir] units of the origin 
      and within the angles [lowthet] and [highthet]*)
  let in_radius coord inr outr lowthet highthet =
    let (x, y) = (float_of_int (fst coord), float_of_int (snd coord)) in
    let angle = atan2 y x in
    let angle = if angle < 0.0 then angle +. (2.0 *. pi) else angle in
    distance_sq coord (0,0) < (square outr)
    && distance_sq coord (0, 0) > (square inr)
    && angle_btwn angle lowthet highthet

  (** [filter_in_radius lst] is the list [lst] filtered to only include 
      coordinates within the inner and outer bow radius'. *)
  let filter_in_radius lst lt ht = 
    List.filter (fun c -> in_radius c bow_ir bow_or lt ht) lst

  (** The coordinates matching the top semi-circle of the attack range. *)
  let top_semi_circle = 
    let rect = make_rect_coords (-bow_or - 1) 1 (2 * bow_or) 
        (bow_or + 1) in 
    filter_in_radius rect (pi /. 3.0) (2.0 *. pi /. 3.0)

  (** The coordinates matching the left semi-circle of the attack range. *)
  let left_semi_circle = 
    let rect = make_rect_coords (-bow_or - 1) (-bow_or - 1) 
        (bow_or + 1) (2 * bow_or) in 
    filter_in_radius rect (5.0 *. pi /. 6.0) (7.0 *. pi /. 6.0)

  (** The coordinates matching the bottom semi-circle of the attack range. *)
  let bottom_semi_circle = 
    let rect = make_rect_coords (-bow_or - 1) (-bow_or - 1) 
        (2 * bow_or + 2) (bow_or + 1) in 
    filter_in_radius rect (4.0 *. pi /. 3.0) (5.0 *. pi /. 3.0)

  (** The coordinates matching the right semi-circle of the attack range. *)
  let right_semi_circle = 
    let rect = make_rect_coords 1 (-bow_or + 1) (bow_or + 1) 
        (2 * bow_or + 2) in
    filter_in_radius rect (11.0 *. pi /. 6.0) (13.0 *. pi /. 6.0)

  let attack w dir = 
    match dir with
    | Action.Up ->    List.map (fun (x, y) -> (x, y, w.damage)) top_semi_circle
    | Action.Down ->  List.map (fun (x, y) -> (x, y, w.damage)) 
                        bottom_semi_circle
    | Action.Left ->  List.map (fun (x, y) -> (x, y, w.damage)) left_semi_circle
    | Action.Right -> List.map (fun (x, y) -> (x, y, w.damage)) 
                        right_semi_circle

end
