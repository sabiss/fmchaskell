{-# LANGUAGE GADTs #-}

module ExList where

import Prelude hiding
    ( (.) , ($)
    , flip , curry , uncurry
    , iterate
    )

-- use your mind to infer the types, don't cheat!
--u n m = f (n, m)-- curry takes a "traditional" binary function

-- u = curry f
-- and returns its currified version
curry :: ((a,b) -> c) -> (a->b->c)
(curry f) n m = f(n,m)--recebo f, que será aplicada em parâmetros n m quaisquer e preciso dizer como ela vai se comportar. Ela vai se comportar em seu modo currificado assim como f se comportaria na sua forma descurrificada

-- uncurry takes a currified function
-- and returns its "traditional" binary version
uncurry :: (a->b->c) -> ((a,b)->c)
(uncurry f) (n,m) = f n m--similar a curry, mas ao contrário :p

-- flip takes a (currified) binary function
-- and returns one that behaves the same but takes its arguments in the opposite order
flip ::(a -> b -> c) -> (b -> a -> c)
(flip f) segundoParametro primeiroParametro = f primeiroParametro segundoParametro--digo ao haskell "qual seria o resultado da minha função f se meus argumentos estivessem trocados? e ele me diz que será o mesmo resultado deles na ordem certa"

-- f flip(f)
-- f (caracter,inteiro) = "oi"
-- (flip (f))(inteiro,caracter) = f(caracter,inteiro) = "oi"

-- (.) takes two composable functions and returns their composition
(.)::(a->b) -> (b->c) -> (a->c)
(f . g) x = g (f x)


-- (.>) is composition but in diagramatic notation (should be ; but Haskell forbids)
(.>) = flip (.)


-- ($) takes a function and a suitable argument and applies the function to the argument
-- think: why would we ever want that?
($) :: (a->b)->a->b --recebo uma função, um agumento e retorno o retorno que a função irá retornar
f $ n = f n

-- iterate: figure it out by its type
iterate :: (a -> a) -> a -> [a]--uma função, deu argumento e iterar novamente o resultado da função+argumento novamente na função. Tipo f(x) = 2x iterable f 2 = f 4 *pq f 2 é 4
iterate f n = f n : iterate f (f n)

-- orbit
orbit = flip iterate

