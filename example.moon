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
  let: (k) -> (v) -> scope[k] = v

eval.eval scope, parser.Expr\match [===[

let 'x'
  'ハロー'
let 'y'
  add(+'100', +'200')
debug.print "\(x)ワールド\(y)"

]===]
