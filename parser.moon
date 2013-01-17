lpeg = require 'lpeg'

pack = (...) -> {...}

foldl1 = (f) -> (...) ->
  x = {...}
  y = x[1]
  for i = 2, #x
    y = f y, x[i]
  return y

foldr1 = (f) -> (...) ->
  x = {...}
  y = x[#x]
  for i = #x - 1, 1, -1
    y = f x[i], y
  return y

swap = (f) -> (y) -> (x) -> (f x) y

show =
  cons: => "(#{@[1]}:#{@[2]})"
  apply: =>
      a = @[2]
      "(#{a[1]}!#{a[2]})"
  tuple: => "(#{table.concat [tostring w for w in *@[2]], ';'})"

cons = (x, y) ->
  if y == nil
    return (y) -> cons x, y
  with z = pack x, y
    setmetatable z,
      __tostring: show.cons
apply = (x, y) ->
  if y == nil
    return (y) -> apply x, y
  with z = pack '!', pack x, y
    setmetatable z,
      __tostring: show.apply
tuple = (x, y, ...) ->
  if y ~= nil
    with z = pack ';', pack x, y, ...
      setmetatable z,
        __tostring: show.tuple
  else
    x

concat = (x, y) ->
  with l = {}
    for v in *x
      table.insert l, v
    for v in *y
      table.insert l, v

count_space = (s) ->
  y = 0
  for i = 1, #s
    switch string.sub(s, i, i)
      when ' ' then y += 1
      when '\t' then y += 4
  return y

Space = lpeg.S' \t'
NewLine = lpeg.P'\n'
LineComment = lpeg.P'--' * (1 - NewLine)^0
Spaces = Space^0 * LineComment^-1
NewLines = NewLine * (Spaces * NewLine)^0
Tab = Space^0 / count_space
CaptureTab = lpeg.Cg Tab, 'tab'
ClearTab = lpeg.Cg (lpeg.Cc 0), 'tab'
Indent = lpeg.Cmt Tab * lpeg.Cb'tab', (s, i, x, y) -> x > y
SameTab = lpeg.Cmt Tab * lpeg.Cb'tab', (s, i, x, y) -> x == y
Wrap = NewLine * #SameTab * Spaces
Offside = (#Indent * CaptureTab) + (#SameTab * Spaces)

Digit = lpeg.R '09'
Alphabet = lpeg.R 'AZ', 'az'
Identifier = lpeg.C (Alphabet + Digit + '_')^1
Sigil = lpeg.C lpeg.S'+-#$@\\'

join = (sep, x) -> x * (sep * x)^0
binary = (op) -> Spaces * op * Spaces
operator = (op, t) -> (join (binary op) * Wrap^-1, t)
emittable = (op, t) -> (join Spaces * (op * Spaces * Wrap^-1)^-1, t)

Unit = '(' * Spaces * ')' / '()'
Square = '[' * Spaces * ']' / '[]'
Curly = '{' * Spaces * '}' / '{}'

Equals = lpeg.P'='^1
LongOpen = '[' * lpeg.Cg(Equals, 'lstr') * '['
LongClose = ']' * lpeg.C(Equals) * ']'
CloseEq = lpeg.Cmt LongClose * lpeg.Cb'lstr', (s, i, x, y) -> x == y
LongString = LongOpen * lpeg.C((1 - CloseEq)^0) * LongClose / (s, o) -> s

Token = Identifier + LongString + Unit + Square + Curly

err = (_, i) ->
  error "Syntax error at position #{i}"

Expr = lpeg.P
  [1]: 'Expr'
  ParenContent: Spaces * NewLines^-1 * ClearTab * Offside * lpeg.V'Blocks' * NewLines^-1 * Spaces
  Paren: '(' * lpeg.V'ParenContent' * ')'
  Bracket: ('[' * lpeg.V'ParenContent' * ']') / apply '[]'
  Brace: ('{' * lpeg.V'ParenContent' * '}') / apply '{}'
  Token: Token + lpeg.V'Paren' + lpeg.V'Bracket' + lpeg.V'Brace'
  Term: (Sigil^0 * lpeg.V'Token' + Sigil) / foldr1 apply
  SCons: (operator lpeg.P'.', lpeg.V'Hanger' + lpeg.V'Term') * Spaces / foldl1 cons
  SApply: (emittable lpeg.P'`', lpeg.V'Hanger' + lpeg.V'SCons') * Spaces / foldl1 apply
  STupleStub: (operator lpeg.P',', lpeg.V'Hanger' + lpeg.V'SApply') * Spaces
  STuple: lpeg.V'STupleStub' * lpeg.V'NewLineContSTuple' * lpeg.P','^-1 / tuple
  WCons: (operator lpeg.P':', lpeg.V'Hanger' + lpeg.V'STuple') * Spaces / foldr1 cons
  WApply: (operator lpeg.P'!', lpeg.V'Hanger' + lpeg.V'WCons') * Spaces / foldr1 apply
  WTupleStub: (operator lpeg.P';', lpeg.V'Hanger' + lpeg.V'WApply') * Spaces
  WTuple: lpeg.V'WTupleStub' * lpeg.V'NewLineContWTuple' * lpeg.P';'^-1 / tuple
  ContSCons: lpeg.P'.' * Spaces * lpeg.V'Term' / swap cons
  ContSApply: lpeg.P'`'^-1 * Spaces * lpeg.V'SCons' / swap apply
  ContSTuple: lpeg.P',' * Spaces * lpeg.V'STupleStub'
  ContWCons: lpeg.P':' * Spaces * lpeg.V'WCons' / swap cons
  ContWApply: lpeg.P'!' * Spaces * lpeg.V'WApply' / swap apply
  ContWTuple: lpeg.P';' * Spaces * lpeg.V'WTupleStub'
  Cont: lpeg.V'ContSCons' + lpeg.V'ContSApply' + lpeg.V'ContWCons' + lpeg.V'ContWApply'
  NewLineCont: (NewLine * Offside * #lpeg.S'.`:!' * join Spaces, lpeg.V'Cont')^0
  NewLineContSTuple: (NewLine * Offside * lpeg.V'ContSTuple')^0
  NewLineContWTuple: (NewLine * Offside * lpeg.V'ContWTuple')^0
  Hanger: NewLines * #Indent * CaptureTab * lpeg.V'Blocks'
  Block: lpeg.V'WTuple' * lpeg.V'NewLineCont' / foldl1 (line, cont) -> cont line
  Blocks: (join NewLines * #SameTab * Spaces, lpeg.V'Block') * Spaces * NewLines^-1 * Spaces / tuple
  Expr: Spaces * NewLines^-1 * CaptureTab * (lpeg.V'Blocks' + lpeg.Cc'()') * (-1 + lpeg.P(err))

return {:Expr, :cons, :apply, :tuple}
