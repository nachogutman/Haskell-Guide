module Test where

import Test.HUnit
import Solution

-- Casos de Test

runHayQueCodificar = runTestTT testsHayQueCodificar

testsHayQueCodificar = test [
    " Caso verdadero : hayQueCodificar 'a' [('a','b')]" ~: (hayQueCodificar 'a' [('a','b')]) ~?= True,
    " Caso falso : hayQueCodificar 'a' [('b','a')]" ~: (hayQueCodificar 'a' [('b','a')]) ~?= False,
    " Caso falso : hayQueCodificar 'a' []" ~: (hayQueCodificar 'a' []) ~?= False,
    " Caso verdadero multiple : hayQueCodificar 'a' [('b','a'),('a','d')]" ~: (hayQueCodificar 'a' [('b','a'),('c','d')]) ~?= True,
    " Caso falso multiple : hayQueCodificar 'a' [('b','a'),('c','d')]" ~: (hayQueCodificar 'a' [('b','a'),('c','d')]) ~?= True
    ]

runCuantasVecesHayQueCodificar = runTestTT testsCuantasVecesHayQueCodificar
testsCuantasVecesHayQueCodificar = test [
    " Caso simple : cuantaVecesHayQueCodificar 'a' ['a','b'] [('a','b')]" ~: (cuantasVecesHayQueCodificar 'a' ['a','b'] [('a','b')]) ~?= 1,
    " Caso multiple : cuantaVecesHayQueCodificar 'a' ['a','b', 'a'] [('a','b')]" ~: (cuantasVecesHayQueCodificar 'a' ['a','b','a'] [('a','b')]) ~?= 2,
    " Caso final: cuantaVecesHayQueCodificar 'a' ['b','b','a'] [('a','b')]" ~: (cuantasVecesHayQueCodificar 'a' ['b','b','a'] [('a','b')]) ~?= 1,
    " Caso falso : cuantaVecesHayQueCodificar 'a' ['b','b','a'] [('b','a')]" ~: (cuantasVecesHayQueCodificar 'a' ['b','b','a'] [('b','a')]) ~?= 0,
    " Caso vacio : cuantaVecesHayQueCodificar 'a' ['b','b','a'] []" ~: (cuantasVecesHayQueCodificar 'a' ['b','b','a'] []) ~?= 0,
    " Caso vacio 2 : cuantaVecesHayQueCodificar 'a' [] [('b','a')]" ~: (cuantasVecesHayQueCodificar 'a' [] [('b','a')]) ~?= 0
    ]


runLaQueMasHayQueCodificar = runTestTT testsLaQueMasHayQueCodificar
testsLaQueMasHayQueCodificar = test [
    " Caso simple : laQueMasHayQueCodificar ['a','b'] [('b','a')]" ~: (laQueMasHayQueCodificar ['a','b'] [('b','a')]) ~?= 'b',
    " Caso repetidos principio : laQueMasHayQueCodificar ['a','a','b','c','c','c'] [('a','b')]" ~: (laQueMasHayQueCodificar ['a','a','b','c','c','c'] [('a','b')]) ~?= 'a',
    " Caso repetidos final : laQueMasHayQueCodificar ['a','a','b','c','c','c'] [('a','b'),('c','d')]" ~: (laQueMasHayQueCodificar ['a','a','b','c','c','c'] [('a','b'),('c','d')]) ~?= 'c',
    " Caso simple 3 : laQueMasHayQueCodificar ['a','b','f','e','f','f','a'] [('c','d'),('f','e'),('a','o')]" ~: (laQueMasHayQueCodificar ['a','b','f','e','f','f','a'] [('c','d'),('f','e'),('a','o')]) ~?= 'f',
    " Caso simple 4 : laQueMasHayQueCodificar ['a','b'] []" ~: (laQueMasHayQueCodificar ['a','b'] []) ~?= 'a'
    ]

runCodificarFrase = runTestTT testsCodificarFrase
testsCodificarFrase = test [
    " Caso simple : codificarFrase ['a','b'] [('a','b')]" ~: (codificarFrase ['a','b'] [('a','b')]) ~?= ['b','b'],
    " Caso simple 2 : codificarFrase ['a','b'] [('a','b'),('b','c')]" ~: (codificarFrase ['a','b'] [('a','b'),('b','c')]) ~?= ['b','c'],
    " Caso simple 3 : codificarFrase ['a','b'] []" ~: (codificarFrase ['a','b'] []) ~?= ['a','b'],
    " Caso simple 4 : codificarFrase [] []" ~: (codificarFrase [] []) ~?= []
    ]