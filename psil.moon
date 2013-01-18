util = require 'util'

import pack, foldl1, foldr1, curry, swap from util

class Tuple
  new: (...) =>
    for k, v in ipairs {...}
      @[k] = v
  map: (f) =>
    with a = @@()
      for k, v in ipairs @
        a[k] = f v
  foldl1: (f) =>
    x = @[1]
    for i = 2, #@
      x = f x, @[i]
    return x
  __tostring: =>
    "(#{table.concat [tostring w for w in *@], ';'})"

class Cons extends Tuple
  fst: => @[1]
  snd: => @[2]
  __tostring: =>
    "(#{@[1]}:#{@[2]})"

class Apply extends Cons
  __tostring: =>
    "(#{@[1]}!#{@[2]})"

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
