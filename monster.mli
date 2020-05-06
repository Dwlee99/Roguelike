type damage = int

type monster = {
  name : string;
  m_type : Board.monster_type;
  position : (int * int);
  health : int;
  max_health : int;
  damage : int;
  action_queue : m_action list;
}
and
  m_action = 
  | Wait of (monster -> monster)
  | Move of (monster -> Board.t -> (int * int) -> (monster))
  | Attack of (monster -> Board.t -> (int * int) -> (monster * damage))

module type Monster_Type = sig

  val create_monster : int -> monster

  val edit_queue : monster -> Board.t -> (int * int) -> m_action list

end

module type Edit_Monster = sig

  val create_monster : int -> monster

  val do_turn : monster -> Board.t -> (int * int) -> (monster * damage)

end

module Make_Monster (M : Monster_Type) : Edit_Monster

val get_type : monster -> Board.monster_type