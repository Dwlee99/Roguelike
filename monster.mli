type damage = int

type monster = {
  name : string;
  id : int;
  health : int;
  max_health : int -> int;
  damage : int -> int;
  action_queue : m_action list;
}
and
  m_action = 
  | Wait of (monster -> monster)
  | Move of (monster -> Board.t -> (monster))
  | Attack of (monster -> Board.t -> (monster * damage))

module type Monster_Type = sig

  val create_monster : unit -> monster

  val edit_queue : monster -> Board.t -> m_action list

end

module type Edit_Monster = sig

  val do_turn : monster -> Board.t -> (monster * damage)

end

module Make_Monster (Type : Monster_Type) : Edit_Monster