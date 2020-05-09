(** [up_one (x, y)] is [(x, y + 1)] *)
val up_one : int * int -> int * int

(** [up_one (x, y)] is [(x, y - 1)] *)
val down_one : int * int -> int * int

(** [up_one (x, y)] is [(x + 1, y)] *)
val right_one : int * int -> int * int

(** [up_one (x, y)] is [(x - 1, y)] *)
val left_one : int * int -> int * int

(** [square x] is [x * x] *)
val square : int -> int

(** [distance_sq (x1, y1) (x2, y2)] is [square (x2 - x1) + square (y2 - y1)] *)
val distance_sq : int * int -> int * int -> int