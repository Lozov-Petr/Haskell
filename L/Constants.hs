module Constants where

cIf          = "if"
cThen        = "then"
cElse        = "else"
cElif        = "elif"

cWhile       = "while"
cDo          = "do"

cRepeat      = "repeat"
cUntil       = "until"  

cFor         = "for"
cTo          = "to"  
cStep        = "step"

cRead        = "read"
cWrite       = "write"

cTry         = "try"
cCatch       = "catch"

cThrow       = "throw"

cAbort       = "abort"

cEOF         = "EOF"

cBreak       = "break"

cContinue    = "continue"

cCreateArray = "createArray"

cSkip        = "skip"

cSwitch      = "switch"
cCase        = "case"
cDefault     = "default"

cEqual       = "="

cDefLabel    = "defaultLabel"

cComment     = "//"

keywordsBegin = [cIf, cWhile, cRead, cTry, cBreak, cContinue]

keywordsOther = [cThen, cElse, cDo, cCatch]

keywordsExpr = [cEOF, cCreateArray]

keywords = keywordsBegin ++ keywordsOther ++ keywordsExpr

voidChars = ['\r', '\n', '\t', ' ']

