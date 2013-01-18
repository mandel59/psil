psil = require 'psil'
util = require 'util'

import curry, compose from util

class Scope
  new: (t) =>
    for k, v in pairs t
      @[k] = v
  index: (name) =>
    if type(name) == 'string' then return rawget @, name
    switch name.__class
      when psil.Cons
        return (@[name\fst()])[name\snd()]

local eval, func

eval = curry (scope, p) ->
  if p == '()' then return nil
  if type(p) == 'string' then return scope\index(p)
  switch p.__class
    when psil.Tuple
      return unpack p\map eval scope
    when psil.Cons
      return scope\index(p)
    when psil.Apply
      f = func scope, p\fst()
      return f eval scope, p\snd()
    when psil.String
      return p[1]

func = curry (scope, p) ->
  if type(p) == 'string' then return scope\index(p)
  switch p.__class
    when psil.Tuple
      return (p\map func scope)\foldl1 compose
    when psil.Cons
      return scope\index(p)
    when psil.Apply
      return eval scope, p

return {:eval, :func, :Scope}
