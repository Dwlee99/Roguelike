open Monster

module Ranger : Edit_Monster = Make_Monster (
  struct
    let max_health floor = floor + 5

    let damage floor = floor / 10 + 1

    let create_monster floor = {
      name = Name.random_name ();
      position = (-1, -1);
      m_type = Board.Ranger;
      max_health = max_health floor;
      health = max_health floor;
      damage = damage floor;
      action_queue = [];
    }

    let wait_function m = {
      m with health = min (m.health + 1) m.max_health 
    }

    let move_function move_to m _ _ = 
      {m with position = move_to}

    let attack_function (targetX, targetY) m b _ =
      let damage = if Board.get_tile b (targetX, targetY) = Board.Player then 1
        else 0 in
      (m, damage)

    (** These functions have self-documenting names. *)
    let up_one (x, y) = (x, y + 1)

    let down_one (x, y) = (x, y - 1)

    let right_one (x, y) = (x + 1, y)

    let left_one (x, y) = (x - 1, y)

    let square x = x * x

    let distance_sq (x1, y1) (x2, y2) =
      square (x2 - x1) + square (y2 - y1)

    let edit_queue monster board (px, py) =
      if List.length monster.action_queue = 0 then 
        let p_pos = (px, py) in
        let m_pos = monster.position in
        let dist_sq = distance_sq p_pos m_pos in
        if dist_sq > 25 then
          let direction = Board.direction_to board monster.position p_pos 10 in
          let c_p = monster.position in
          match direction with
          | Some d -> begin
              match d with
              | Up -> [Move (move_function (up_one c_p))]    
              | Down -> [Move (move_function (down_one c_p))]
              | Left -> [Move (move_function (left_one c_p))]
              | Right -> [Move (move_function (right_one c_p))]
            end
          | None -> [Wait (wait_function)]
        else begin
          [Wait (wait_function); Attack (attack_function p_pos)]
        end
      else 
        monster.action_queue
  end
  )
