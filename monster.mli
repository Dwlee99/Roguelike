type damage = int

type monster = {
  name : string;
  position : (int * int);
  m_type : Board.monster_type;
  health : int;
  max_health : int;
  damage : int;
  action_queue : m_action list;
  roaming_target : (int * int) ref;
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

val up_one : int * int -> int * int

val down_one : int * int -> int * int

val right_one : int * int -> int * int

val left_one : int * int -> int * int

val square : int -> int

val distance_sq : int * int -> int * int -> int

val get_roam_direction : monster -> Board.t -> int -> Action.direction