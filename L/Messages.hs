module Messages where

import Constants
import Types

--------------------------
mExpected :: String -> ErrorMessage
--------------------------
mExpected a = Just $ "Expected " ++ a ++ "."


--------------------------
mExpectedNumber :: ErrorMessage
--------------------------
mExpectedNumber = mExpected "number"


--------------------------
mExpectedVariable :: ErrorMessage
--------------------------
mExpectedVariable = mExpected "variable"


--------------------------
mEOF :: ErrorMessage
--------------------------
mEOF      = Just "File ended."