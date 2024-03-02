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




-- Property based testing (verificar ficheiro PropertyBasedTesting para propriedades que aceitam geradores)
-- prop_PrintParse
-- prop_OptInnermostTP
-- prop_SmellCommutativeOpt


-- testar a propriedade definida pela função prop
--   prop :: PicoC -> Bool
--   prop ast = ast == parser (unparse ast)
-- (precisam usar deriving Eq nos datas types)

-- -> fazer parsing apos o pretty printing de uma ast, produz essa mesma ast
-- -> testar se diferentes estrat ́egias de otimiza ̧c ̃ao de express ̃oes aritméticas são equivalentes
