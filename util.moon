return {
  pack: (...) -> {...}
  foldl1: (f) -> (...) ->
    x = {...}
    y = x[1]
    for i = 2, #x
      y = f y, x[i]
    return y
  foldr1: (f) -> (...) ->
    x = {...}
    y = x[#x]
    for i = #x - 1, 1, -1
      y = f x[i], y
    return y
  map: (f) -> (...) ->
    x = {...}
    y = {}
    for v in *x
      table.insert y, f v
    return unpack y
  curry: (f) -> (x, y, ...) ->
    if y == nil
      (...) -> f x, ...
    else
      f x, y, ...
  swap: (f) -> (y) -> (x) -> (f x) y
  compose: (f, g) -> (...) -> g f ...
}
