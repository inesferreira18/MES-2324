import Unparser
import Parser
import PicoC
import Tests
--import Opt


-- Parser
parse :: String -> PicoC
parse p = fst $ last $ pPicoC p

-- Unparser
unparse :: PicoC -> String
unparse = upPicoC

-- Optimazations
--optimazations:: PicoC -> PicoC
--optimazations = opt

--parseWithOpt :: String -> PicoC
--parseWithOpt s = opt (parse s)
