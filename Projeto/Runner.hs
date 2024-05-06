module Runner where

import PicoC
import Eval

--------------------------------------------------------
-- Runner Functions
--------------------------------------------------------



runTestSuite :: PicoC -> [([Int], Int)] -> Bool
runTestSuite p ts = all (runTest p) ts

runTest :: PicoC -> ([Int], Int) -> Bool
runTest p (inputs, expected) = evaluate p inputs == expected