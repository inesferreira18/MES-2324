import Unparser
import Parser
import PicoC
import Tests
import Opt
import Prop
import TestGenerator
import Test.QuickCheck

-- Parser
parse :: String -> PicoC
parse p = fst $ last $ pPicoC p

-- Unparser
unparse :: PicoC -> String
unparse = upPicoC

-- Optimazations
optimazations:: PicoC -> PicoC
optimazations = optIM

parseWithOpt :: String -> PicoC
parseWithOpt s = optIM (parse s)

-- Tests Generator
