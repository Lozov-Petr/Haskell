module Constants where

cIf          = "if"
cThen        = "then"
cElse        = "else"

cWhile       = "while"
cDo          = "do"

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

cEqual       = "="

cDefLabel    = "defaultLabel"

cComment     = "//"

keywordsBegin = [cIf, cWhile, cRead, cTry, cBreak, cContinue]

keywordsOther = [cThen, cElse, cDo, cCatch]

keywordsExpr = [cEOF, cCreateArray]

keywords = keywordsBegin ++ keywordsOther ++ keywordsExpr

voidChars = ['\r', '\n', '\t', ' ']

