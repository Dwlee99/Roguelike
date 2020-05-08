open Monster
open Random

let () = Random.self_init ()

module Swordsman : Edit_Monster = Make_Monster (
  struct
    let max_health floor = floor * 2 + 5

    let damage floor = floor / 10 * 2 + 1

    let create_monster floor = {
      name = Name.random_name ();
      position = (-1, -1);
      m_type = Board.Swordsman;
      max_health = max_health floor;
      health = max_health floor;
      damage = damage floor;
      action_queue = [];
      roaming_target = ref (-1, -1);
    }

    let wait_function m = {
      m with health = min (m.health + 1) m.max_health 
    }

    let move_function move_to m _ _ = 
      {m with position = move_to}

    let attack_function m _ _ =
      (m, m.damage)

    (** These functions have self-documenting names. *)
    let up_one (x, y) = (x, y + 1)

    let down_one (x, y) = (x, y - 1)

    let right_one (x, y) = (x + 1, y)

    let left_one (x, y) = (x - 1, y)

    let square x = x * x

    let distance_sq (x1, y1) (x2, y2) =
      square (x2 - x1) + square (y2 - y1)

    let move_direction d c_p =
      match d with
      | Action.Up -> [Move (move_function (up_one c_p))]    
      | Action.Down -> [Move (move_function (down_one c_p))]
      | Action.Left -> [Move (move_function (left_one c_p))]
      | Action.Right -> [Move (move_function (right_one c_p))]

    let get_random_tile x y r =
      let x_rel = Random.int (2 * r) in 
      let y_rel = Random.int (2 * r) in 
      (x + x_rel - r, y + y_rel - r)

    let rec get_new_roaming_target monster board r =
      let monster_x = fst monster.position in 
      let monster_y = snd monster.position in 
      let new_target = get_random_tile monster_x monster_y r in
      match Board.direction_to board monster.position new_target r with
      | Some _ -> new_target
      | None -> get_new_roaming_target monster board r

    let rec get_roam_direction monster board r = 
      let direction_to_roam = 
        Board.direction_to board monster.position !(monster.roaming_target) r in 
      match direction_to_roam with
      | Some d -> d
      | None -> 
        monster.roaming_target := get_new_roaming_target monster board r; 
        get_roam_direction monster board r

    let edit_queue monster board (px, py) =
      let p_pos = (px, py) in
      let m_pos = monster.position in
      let dist_sq = distance_sq p_pos m_pos in
      if dist_sq > 1 then
        let direction = Board.direction_to board monster.position p_pos 10 in
        let c_p = monster.position in
        match direction with
        | Some d -> move_direction d c_p
        | None -> if (Random.int 5) = 0 then [Wait (wait_function)]
          else move_direction (get_roam_direction monster board 10) c_p
      else begin
        [Attack attack_function]
      end
  end
  )
