util = require 'util'
psil = require 'psil'
lpeg = require 'lpeg'

import pack, foldl1, foldr1, curry, swap from util

show =
  cons: => "(#{@[1]}:#{@[2]})"
  apply: =>
      a = @[2]
      "(#{a[1]}!#{a[2]})"
  tuple: => "(#{table.concat [tostring w for w in *@[2]], ';'})"

cons = curry psil.Cons
apply = curry psil.Apply
tuple = (x, y, ...) ->
  if y ~= nil
    psil.Tuple x, y, ...
  else
    x

count_space = (s) ->
  y = 0
  for i = 1, #s
    switch string.sub(s, i, i)
      when ' ' then y += 1
      when '\t' then y += 4
  return y

psilstr = (...) ->
  psil.String ((foldl1 (x, y) -> x .. y) '', ...)

unescape =
  ['\n']: '\n'
  ['a']: '\a'
  ['b']: '\b'
  ['f']: '\f'
  ['n']: '\n'
  ['r']: '\r'
  ['t']: '\t'
  ['v']: '\v'
  ['\\']: '\\'
  ['\"']: '\"'
  ['\'']: '\''

Digit = lpeg.R '09'
Hex = lpeg.R '09', 'AF', 'af'
Alphabet = lpeg.R 'AZ', 'az'
Identifier = lpeg.C (Alphabet + Digit + '_')^1
Sigil = lpeg.C lpeg.S'+-#$@\\'

Hex4 = Hex * Hex * Hex * Hex

CharCodeDec = lpeg.C(Digit * Digit^-2) / (d) -> string.char tonumber d, 10
CharCodeHex = 'x' * lpeg.C(Hex * Hex) / (x) -> string.char tonumber x, 16
NamedSequence = lpeg.C(lpeg.S'\nabfnrtv\\\"\'') / unescape
Escape = lpeg.P'\\' * (CharCodeDec + CharCodeHex + NamedSequence)
String = '\'' * ((Escape + (lpeg.C(1) - lpeg.S'\n\\\''))^0 / psilstr) * '\''

Equals = lpeg.P'='^1
LongOpen = '[' * lpeg.Cg(Equals, 'lstr') * '['
LongClose = ']' * lpeg.C(Equals) * ']'
CloseEq = lpeg.Cmt LongClose * lpeg.Cb'lstr', (s, i, x, y) -> x == y
LongString = LongOpen * lpeg.C((1 - CloseEq)^0) * LongClose / (s, o) -> psil.String s

Space = lpeg.S' \t'
NewLine = lpeg.P'\n'
LineComment = lpeg.P'--' * (1 - NewLine)^0
Spaces = Space^0 * LineComment^-1
NewLines = NewLine * (Spaces * NewLine)^0
Tab = Space^0 / count_space
CaptureTab = lpeg.Cg Tab, 'tab'
ClearTab = lpeg.Cg (lpeg.Cc 0), 'tab'
MoreTab = lpeg.Cmt Tab * lpeg.Cb'tab', (s, i, x, y) -> x > y
SameTab = lpeg.Cmt Tab * lpeg.Cb'tab', (s, i, x, y) -> x == y
Indent = #MoreTab * CaptureTab
Codent = #SameTab * Spaces
Wrap = NewLine * Codent
Offside = Indent + Codent

Unit = '(' * Spaces * ')' / '()'
Square = '[' * Spaces * ']' / -> apply '[]', '()'
Curly = '{' * Spaces * '}' / -> apply '{}', '()'

join = (sep, x) -> x * (sep * x)^0
binary = (op) -> Spaces * op * Spaces
operator = (op, t) -> (join (binary op) * Wrap^-1, t)
emittable = (op, t) -> (join Spaces * (op * Spaces * Wrap^-1)^-1, t)

err = (_, i) ->
  error "Syntax error at position #{i}"

Expr = lpeg.P
  [1]: 'Expr'
  ParenContent: Spaces * NewLines^-1 * ClearTab * Offside * lpeg.V'Blocks' * NewLines^-1 * Spaces
  Paren: '(' * lpeg.V'ParenContent' * ')'
  Bracket: ('[' * lpeg.V'ParenContent' * ']')
  Brace: ('{' * lpeg.V'ParenContent' * '}')
  Parens: Unit + Square + Curly + lpeg.V'Paren' + lpeg.V'Bracket' / (apply '[]') + lpeg.V'Brace' / (apply '{}')
  EmbedParen: '\\' * lpeg.V'Parens'
  EmbedString: lpeg.P'""' / (-> apply '""', '()') + '"' * ((((Escape + (lpeg.C(1) - lpeg.S'\n\\\"'))^1 / psilstr) + lpeg.V'EmbedParen')^1 / tuple) / (apply '""') * '"'
  Token: Identifier + String + LongString + lpeg.V'EmbedString' + lpeg.V'Parens'
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
  Hanger: NewLines * Indent * lpeg.V'Blocks'
  Block: lpeg.V'WTuple' * lpeg.V'NewLineCont' / foldl1 (line, cont) -> cont line
  Blocks: (join NewLines * Codent, lpeg.V'Block') * Spaces / tuple
  Expr: (Spaces * NewLines)^-1 * CaptureTab * (lpeg.V'Blocks' + lpeg.Cc'()') * NewLines^-1 * Spaces * (-1 + lpeg.P(err))

return {:Expr}
