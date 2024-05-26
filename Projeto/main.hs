{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Unparser
import Parser
import PicoC
import Tests
import Opt
import Prop
import TestGenerator
import Test.QuickCheck
import Eval
import MutationGenerator
import Instrument

-----------
-- Parse --
-----------
parse :: String -> PicoC
parse p = fst $ last $ pPicoC p


-------------
-- Unparse --
-------------
unparse :: PicoC -> String
unparse = upPicoC


-------------------
-- Optimizations --
-------------------
optimizationsTD:: PicoC -> PicoC
optimizationsTD = optTD

parseWithOptTD :: String -> PicoC
parseWithOptTD s = optTD (parse s)


optimizationsIM:: PicoC -> PicoC
optimizationsIM = optIM

parseWithOptIM :: String -> PicoC
parseWithOptIM s = optIM (parse s)


-----------
-- Tests --
-----------
testGen :: Int -> Int -> Int -> IO PicoC
testGen numFuncs numInsts maxExps = generate (genPicoC numFuncs numInsts maxExps)

unparseTest :: IO PicoC -> IO ()
unparseTest ioPicoC = do
                        picoC <- ioPicoC
                        let str = unparse picoC
                        putStrLn str


-----------------
-- Refactoring --
-----------------
parseWithRefactoring :: String -> PicoC
parseWithRefactoring s = refactor (parse s)

refactoring :: PicoC -> PicoC
refactoring = refactor


---------------
-- Mutations --
---------------
mutation :: PicoC -> IO PicoC
mutation program = generate (mutate program)

unparseMutation :: IO PicoC -> IO String
unparseMutation ioPicoC = do
                            picoC <- ioPicoC
                            let str = unparse picoC
                            return str


------------
-- Runner --
------------
{-
int main(int a){ 
    int margem = a;
    if (margem > 30)
    then { margem = 4 * 23 + 3 ; }
    else { margem = 0; }
    return margem; 
}
-}
programa1 = "int main(int a){int margem=a; if (margem>30) then{margem=4*23+3;} else{margem = 0;} return margem;}"
programa1_mutado = "int main(int a){int margem=a; if(margem>30) then{margem=(4*23)/3;} else{margem=0;} return margem;}"
testSuitePrograma1 :: [([Int], Int)]
testSuitePrograma1 = [([32], 95),([29],0),([-1],0),([100],95),([1],0)]

{-
int main(int y, int z){ 
    bool teste = False;
    for(int i=0; i<5 && !teste; i=i+1){
        y = y + i * z;
    }
    if(y>60){
        y = y - 25;    
    }
    return y;
}
-}
programa2 = "int main(int y, int z){bool teste=False; for(int i=0; i<5&&(!teste); i=i+1){y = y+i*z;}if(y>60){y=y-25;} return y;}"
programa2_mutado = "int main(int y, int z){bool teste=False; for(int i=0; i<=5&&(!teste); i=i+1){y = y+i*z;}if(y>60){y=y-25;} return y;}"
testSuitePrograma2 :: [([Int], Int)]
testSuitePrograma2 = [([0,2], 20),([-2,2], 18),([1,1], 11),([10,-1], 0),([10,5], 60)]

{-
int main(int a){ 
    int c;
    c=2+1;
    if(a < 3) then{
        int i=0; 
        int j=5; 
        while(i<7){ 
            j=j-1; 
            i=i+1;
        }
        c = c * j;
    } else {
        c = 0;
    }
    print(" CHEGUEI AQUI! ");
    return c;
}
-}
programa3 = "int main(int a){int c; c=2+1; if(a<3) then{int i=0; int j=5; while(i<7){ j=j-1; i=i+1;} c=c*j;} else {c=0;} print(\" CHEGUEI AQUI! \"); return c;}"
programa3_mutado = "int main(int a){ int c; c=2+1; if(a<3) then{int i=0; int j=83; while(i<7) {j=j-1; i=i+1;} c=c*j; } else{c=0;} print(\" CHEGUEI AQUI! \"); return c; }"
testSuitePrograma3 :: [([Int], Int)]
testSuitePrograma3 = [([0], -6), ([4], 0),([-1], -6),([3], 0),([5], 0)]

run :: String -> [([Int], Int)] -> Bool
run prog testSuite = runTestSuite (parse prog) testSuite

runMutation :: String -> [([Int], Int)] -> IO Bool
runMutation prog testSuite = do 
                                let picoC = parse prog
                                mutatedPicoC <- unparseMutation (mutation picoC)
                                putStrLn mutatedPicoC
                                return $ runTestSuite (parse mutatedPicoC) testSuite


---------------------
-- Instrumentation --
---------------------
instrument :: PicoC -> PicoC
instrument p = instrumentation p

instrumentTestSuite :: String -> [([Int],Int)] -> Bool
instrumentTestSuite prog testSuite = instrumentedTestSuite (parse prog) testSuite

