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

-- Optimizations
optimizationsTD:: PicoC -> PicoC
optimizationsTD = optTD

parseWithOptTD :: String -> PicoC
parseWithOptTD s = optTD (parse s)


optimizationsIM:: PicoC -> PicoC
optimizationsIM = optIM

parseWithOptIM :: String -> PicoC
parseWithOptIM s = optIM (parse s)

-- Tests Generator
testGen :: Int -> Int -> Int -> IO PicoC
testGen numFuncs numInsts maxExps = generate (genPicoC numFuncs numInsts maxExps)

unparseTest :: IO PicoC -> IO ()
unparseTest ioPicoC = do
                        picoC <- ioPicoC
                        let str = unparse picoC
                        putStrLn str

-- Refactoring
parseWithRefactoring :: String -> PicoC
parseWithRefactoring s = refactor (parse s)

refactoring :: PicoC -> PicoC
refactoring = refactor
