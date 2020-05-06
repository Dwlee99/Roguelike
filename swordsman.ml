open Monster

module Swordsman : Edit_Monster = Make_Monster (
  struct
    let max_health floor = floor * 2 + 5

    let damage floor = floor / 10 * 2 + 1

    let create_monster floor = {
      name = "Swordsman";
      position = (-1, -1);
      m_type = Board.Swordsman;
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

    (** These functions have self-documenting names. *)
    let up_one (x, y) = (x, y + 1)

    let down_one (x, y) = (x, y - 1)

    let right_one (x, y) = (x + 1, y)

    let left_one (x, y) = (x - 1, y)

    let edit_queue monster board (px, py) =
      let p_pos = (px, py) in
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

  end
  )
