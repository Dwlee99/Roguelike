type protection = int

type armor = {
  name : string;
  protection : protection;
  level : int;
}

val create_armor: int -> armor