type protection = int

type armor = {
  name : string;
  protection: protection;
  level: int;
}

let create_armor level =
  {
    name = ("level " ^ (string_of_int level) ^ " armor");
    protection = level;
    level = level;
  }