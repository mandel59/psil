psil = require 'psil'
parser = require 'parser'
eval = require 'eval'
import foldl1 from require 'util'

local scope
scope = eval.Scope
  ['""']: (...) ->
    table.concat [tostring v for v in *{...}]
  ['+']: tonumber
  add: foldl1 (x, y) -> x + y
  debug:
    print: print
  let: eval.Macro (scope, p) =>
    switch p.__class
      when psil.Tuple
        for v in *p
          @ scope, v
      when psil.Cons
        scope\newindex p\fst(), eval.eval scope, p\snd()
  ['{}']: eval.Macro (scope, p) =>
    x = {}
    switch p.__class
      when psil.Tuple
        for v in *p
          x[v\fst()] = eval.eval scope, v\snd()
      when psil.Cons
        x[p\fst()] = eval.eval scope, p\snd()
    return x
        
  _G: _G

eval.eval scope, parser.Expr\match io.read '*a'
