open Monster
open Random
open Math

let () = Random.self_init ()

module Sniper : Edit_Monster = Make_Monster (
  struct
    (** [max_health f] is the max health of a sniper on floor [f]. *)
    let max_health floor = 4 * floor - 3

    (** [damage f] is the sniper's damage on floor [f]. *)
    let damage floor = floor / 3 + 1

    (** [exp f] is the amount of exp a sniper gives on floor [f]. *)
    let exp floor = floor / 5 + 1

    let create_monster floor = {
      name = Name.random_name ();
      position = (-1, -1);
      m_type = Board.Sniper;
      max_health = max_health floor;
      health = max_health floor;
      damage = damage floor;
      exp = exp floor;
      action_queue = [];
      roaming_target = ref (-1, -1);
    }

    (** [wait_function m] is what the sniper does when it waits. *)
    let wait_function m = m

    (** [move_function m mons] is what the sniper does when it moves. *)
    let move_function move_to m _ _ = 
      {m with position = move_to}

    (** The range at which a sniper can hit its target. *)
    let s_range = 40

    (** [d_coords m dir] are the coordinates able to be hit by the sniper and
        their associated damages. *)
    let d_coords m dir b =
      let (x, y) = m.position in
      let damage = ref (3 * m.damage) in
      let (x_displace, y_displace) = 
        match dir with
        | Action.Up -> (0, 1)
        | Action.Left -> (-1, 0)
        | Action.Down -> (0, -1)
        | Action.Right -> (1, 0) in
      List.init s_range (fun i -> 
          let (to_x, to_y) = (x + x_displace * i, y + y_displace * i)
          in
          if Board.get_tile b (to_x, to_y) = Wall true then 
            damage := max 0 (!damage - m.damage);
          (to_x, to_y, !damage)
        )

    (** [attack dir m b p_pos] is what the sniper does when it 
        attacks. It shoots the coordinates in [lst] and does the damage listed 
        at the coordinate. *)
    let attack dir m b p_pos  =
      let dam_coords = d_coords m dir b in
      let d_opt = List.find_opt (fun (x, y, _) -> p_pos = (x, y)) dam_coords in
      match d_opt with
      | Some (_, _, d) -> (m, d)
      | None -> (m, 0)

    (** [move_direction dir cur_pos] is the [m_action] that moves the monster 
        in [dir]. *)
    let move_direction d c_p =
      match d with
      | Action.Up -> [Move (move_function (up_one c_p)); 
                      Attack (attack Action.Up)]   
      | Action.Down -> [Move (move_function (down_one c_p));
                        Attack (attack Action.Down)]
      | Action.Left -> [Move (move_function (left_one c_p));
                        Attack (attack Action.Left)]
      | Action.Right -> [Move (move_function (right_one c_p));
                         Attack (attack Action.Right)]

    let edit_queue monster board (px, py) =
      let p_pos = (px, py) in
      let m_pos = monster.position in
      if List.length monster.action_queue = 0 then
        let direction = Board.direction_to board m_pos p_pos 10 in
        match direction with
        | Some d -> if (Random.int 5) = 0 then [Wait (wait_function)] 
          else move_direction d m_pos
        | None -> if (Random.int 5) = 0 then [Wait (wait_function)]
          else move_direction (get_roam_direction monster board 10) m_pos
      else begin
        monster.action_queue
      end
  end
  )
