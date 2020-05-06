open Monster

module Swordsman : Edit_Monster = Make_Monster (
  struct
    let max_health floor = floor * 2 + 5

    let damage floor = floor / 10 * 2 + 1

    let create_monster floor = {
      name = "Swordsman";
      position = None;
      max_health = max_health floor;
      health = max_health floor;
      damage = damage floor;
      action_queue = [];
    }

    let wait_function m = {
      m with health = min (m.health + 1) m.max_health 
    }

    let edit_queue monster board p_pos = [Wait wait_function]

  end
  )