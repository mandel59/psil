class Cons
  new: (x, y) =>
    @[1] = x
    @[2] = y
  fst: => @[1]
  snd: => @[2]
  __tostring: =>
    "(#{@[1]}:#{@[2]})"

class Apply extends Cons
  __tostring: =>
    "(#{@[1]}!#{@[2]})"

class Tuple
  new: (...) =>
    for k, v in ipairs {...}
      @[k] = v
  map: (f) =>
    @@ unpack [f v for v in *@]
  __tostring: =>
    "(#{table.concat [tostring w for w in *@], ';'})"

class String
  escape_sequence =
    ['\a']: '\\a'
    ['\b']: '\\b'
    ['\f']: '\\f'
    ['\n']: '\\n'
    ['\r']: '\\r'
    ['\t']: '\\t'
    ['\v']: '\\v'
    ['\\']: '\\\\'
    ['\"']: '\\\"'
    ['\'']: '\\\''
  new: (s) =>
    @[1] = s
  escape: =>
    x = string.gsub @[1], "(.)", escape_sequence
    string.gsub x, "(%c)", (c) -> "\\#{string.byte(c)}"
  __tostring: =>
    "'#{@escape()}'"

return {
  :Cons
  :Apply
  :Tuple
  :String
}
