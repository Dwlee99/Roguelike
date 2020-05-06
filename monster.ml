type damage = int

type monster_type = 
  | Swordsman

type monster = {
  name : string;
  position : State.coordinate option;
  health : int;
  max_health : int;
  damage : int;
  action_queue : m_action list;
}
and
  m_action = 
  | Wait of (monster -> monster)
  | Move of (monster -> Board.t -> State.coordinate -> (monster))
  | Attack of (monster -> Board.t -> State.coordinate -> (monster * damage))

module type Monster_Type = sig

  val create_monster : int -> monster

  val edit_queue : monster -> Board.t -> State.coordinate -> m_action list

end

module type Edit_Monster = sig

  val create_monster : int -> monster

  val do_turn : monster -> Board.t -> State.coordinate -> (monster * damage)

end

module Make_Monster (M : Monster_Type) : Edit_Monster = struct

  let create_monster = M.create_monster

  let do_action action monster board p_pos = 
    match action with
    | Wait f -> (f monster, 0)
    | Move f -> (f monster board p_pos, 0)
    | Attack f -> f monster board p_pos

  let do_turn monster board p_pos =
    let m = {monster with action_queue = M.edit_queue monster board p_pos} in
    match m.action_queue with
    | [] -> (m, 0)
    | h::t -> do_action h {monster with action_queue = t} board p_pos

end


