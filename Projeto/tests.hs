module Tests where

test1 :: String
test1 = "int aux=0 / 2;\n\n\nint aux = 0 / 2;}"

test2 :: String
test2 = "int x = 2; x = (3 + 1) * 2; x = x * 1;"

test3 :: String
test3 = "while(i < 7){int x;}"

test4 :: String
test4 = "while(0){ int x; }"

test5 :: String
test5 = "while(33){ int x; }"

test6 :: String
test6 = "int margem = 15; \
       \ if (margem > 30) \
       \ then { margem = 4 * 23 + 3 ; } \
       \ else { margem = 0; }"

test7 :: String
test7 = "while (margem < 30) { margem = margem + 1; } \
       \ altura = -2 / aux; \
       \ centrado = True;"

test8 :: String
test8 = "int x = 2*1;string st; st = ola; bool y; int x = x + 0 + 0; y=True;"

test9 :: String
test9 = "if (margem > 30 && margem <= 50) \
       \ then { margem = 4 * 23 + 3 ; } \
       \ else { margem = 0; }\n\n "

--test10 :: String
--test10 = "if(i + x < 3)then{while(i < 7){i=i-1;}}else{c = 0;}" 