module Tests where

test1 :: String
test1 = "char calcula(){int aux=0 / 2;\n\n\nint aux = 0 / 2;}"

test2 :: String
test2 = "char funcao(char ca){int x = 2; x = (3 + 1) * 2; ca = a;}"

test3 :: String
test3 = "void main(){while(i < 7){int x;}}"

test4 :: String
test4 = "void main(){  while(0){ int x; }}"

test5 :: String
test5 = "int main(string a){while(33){ int x; } return a; }"

test6 :: String
test6 = "int main(int a){ int margem = 15; \
       \ if (margem > 30) \
       \ then { margem = 4 * 23 + 3 ; } \
       \ else { margem = 0; } }"

test7 :: String
test7 = "int main(int a, char b){while (margem < 30) { margem = margem + 1; } \
       \ altura = -2 / aux; \
       \ centrado = True;}"

test8 :: String
test8 = "int main(){int x = 2*1;string st; st = ola; bool y; int x = x + 0 + 0; y=True;}"

test9 :: String
test9 = "void func(){if (margem > 30 && margem <= 50) \
       \ then { margem = 4 * 23 + 3 ; } \
       \ else { margem = 0; }\n\n }"

test10 :: String
test10 = "void forloop(){for(int i = 0, x =10; i < 3 && x>0 ; i = i + 1, x = x-1){}}"   

test11 :: String
test11 = "int main() {for(;2;){int x;} for(int i = 0; i < 30; i = i+1){ y = z * 5; }}"

test12 :: String
test12 = "void main(){ if(i + x < 3) then{ while(i < 7){ i=i-1; } } else {c = 0; }}" 

test13 :: String
test13 = "int main(){ int a = func1(a,b); }"

test14 :: String
test14 = "int main(){ int teste = 3; bool teste2 = True; if(!teste) then { int x = 2; } else { int y = 3; } return y; }"   

test15 :: String
test15 = "int main(){ a = func1(a,b); b = func2(c); while(i < 7){ i=i-1; } int d = 5 + 2 * x;}" 

test16 :: String
test16 = "int main(){if(3<5) {string x= 2; if (y<x){a = func2(c);}} return a + 2; }   void func(int a) {x= y+4;}"

test17:: String
test17 = "int main(){for(int i=0, j=1;i<10;i = i+1){ while(i<2){x=2+s; if(2<3) then {x=2;} else {x=3;}}}}"