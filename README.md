# MES-2324

Repositório destinado a guardar o trabalho realizado na cadeira de Manutenção e Evolução de Software no ano letivo 2023/2024.

## Grupo
| Número   | Nome                                                        |
| -------- | ----------------------------------------------------------- |
| PG53879  | [Inês Ferreira](https://github.com/inesferreira18) |
| PG54084  | [Marta Sá](https://github.com/findingmarta)                 |

### Execução

 Para executar o programa basta correr o seguinte comando:

```stack ghci main.hs```

### Comandos

```parse :: String -> PicoC``` - faz o parse de uma determinada string

```unparse :: PicoC -> String``` - faz o unparse de um programa PicoC


```optimizationsTD :: PicoC -> PicoC``` - aplica optimizações a um programa PicoC, seguindo uma estratégia Full Top-Down

```optimizationsIM :: PicoC -> PicoC``` - aplica optimizações a um programa PicoC, seguindo uma estratégia Innermost

```optimizationsIM :: String -> PicoC``` - faz o parse de uma determinada string e aplica optimizações ao respetivo programa PicoC, seguindo uma estratégia Innermost


```testGen :: Int -> Int -> Int -> IO PicoC``` - gera um programa PicoC aleatório definindo o número de funções, instruções e expressões pretendidas 

```unparseTest :: IO PicoC -> IO ()``` - faz o unparse do programa PicoC gerado


```refactoring :: PicoC -> PicoC``` - aplica operações de refactoring a um programa PicoC

```parseWithRefactoring :: String -> PicoC``` - faz o parse de uma determinada string e aplica operações de refactoring ao respetivo programa PicoC


```mutation :: PicoC -> IO PicoC``` - gera uma mutação num programa PicoC

```unparseMutation :: IO PicoC -> IO String``` - faz o unparse do programa PicoC com a mutação gerada


```run :: String -> [([Int], Int)] -> Bool``` - recebe uma lista de inputs do programa e respetivo resultado e verifica se a execução do programa produz os resultados esperados

```runMutation :: String -> [([Int], Int)] -> IO Bool``` - recebe uma lista de inputs do programa e respetivo resultado e verifica se a execução de um programa mutado produz resultados diferentes dos esperados


```instrument :: PicoC -> PicoC``` - faz a instrumentação de um programa PicoC

```instrumentTestSuite :: String -> [([Int],Int)] -> Bool``` - faz a instrumentação de um programa PicoC e executando de seguida para avaliar se os resultados produzidos são iguais aos esperado

### Testes

No módulo ```Tests.hs``` tem alguns exemplos com frases válidas da linguagem, sendo que os mesmos podem ser chamados no módulo principal.

Exemplo: ```parse test_5```

Nota Final: 16.5
