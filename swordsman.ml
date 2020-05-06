open Monster

module Swordsman : Edit_Monster = Make_Monster (
  struct
    let max_health floor = floor * 2 + 5

    let damage floor = floor / 10 * 2 + 1

    let create_monster floor = {
      name = "Swordsman";
      m_type = Board.Swordsman;
      position = (-1, -1);
      max_health = max_health floor;
      health = max_health floor;
      damage = damage floor;
      action_queue = [];
    }

    let wait_function m = {
      m with health = min (m.health + 1) m.max_health 
    }

    let move_function move_to m _ _ = 
      let (x, y) = m.position in
      {m with position = (x, y + 1)}
    let edit_queue monster board p_pos =
      let path_list = Some [5; 3] in
      match path_list with
      | Some (h::t) -> [Move (move_function h)]
      | _ -> [Wait (wait_function)]

  end
  )