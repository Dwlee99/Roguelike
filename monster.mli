type damage = int

type monster = {
  name : string;
  position : State.coordinate;
  health : int;
  max_health : State.floor -> int;
  damage : State.floor -> int;
  action_queue : m_action list;
}
and
  m_action = 
  | Wait of (monster -> monster)
  | Move of (monster -> Board.t -> (Board.t * monster))
  | Attack of (monster -> Board.t -> (damage * monster))

module type Monster_Type = sig

  val create_monster : unit -> monster

  val edit_queue : monster -> Board.t -> m_action list

end

module type Edit_Monster = sig

  val do_turn : monster -> Board.t -> (monster * Board.t * damage)

end

module Make_Monster (Type : Monster_Type) : Edit_Monster