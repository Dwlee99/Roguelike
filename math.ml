let up_one (x, y) = (x, y + 1)

let down_one (x, y) = (x, y - 1)

let right_one (x, y) = (x + 1, y)

let left_one (x, y) = (x - 1, y)

let square x = x * x

let distance_sq (x1, y1) (x2, y2) =
  square (x2 - x1) + square (y2 - y1)