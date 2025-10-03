{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-} 
-- elemento : lista
head :: [a] -> a--pega uma lista e retorna o 1° item dela
head [] = error "lista va"
head (x:xs)= x

tail :: [a] -> [a]
tail [] = error "lista vazia"
tail (_:xs) = xs

null :: [a] -> Bool
null []= True
null [_] = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs 

product :: Num a => [a] -> a
product [ ]= 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [ ] = [ ]
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x []= [x]
snoc x (y:ys) = y : snoc x ys --tiro todos os elementos de ys até deixar ele vazio, quando coloco o x dentro. Após deixar ys vazia, adicionar o x nele, saio adicionando os y que havia tirado

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y--adiciona y no final de xs
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []= error "lista vazia"
minimum [unicoElemento] = unicoElemento 
minimum (x:xs) = min x (minimum xs)--faça o minimo entre o 1° elemento da lista e seu restando
-- minimum 5:[3,4] = min 5 (minimum [3,4])=3
--                            min 3 (minimum [4])=3
--                               

maximum :: Ord a => [a] -> a
maximum [ ] = error "Lista vazia bro"
maximum [unicoElemento] = unicoElemento
maximum (x:xs) = max x (maximum xs)--semelhante a minimum, eu vou deixando xs cada vez menor até sobrar apenas um elemento dentro e, após isso, vou aplicando max neles 

take :: Int -> [a] -> [a]
take 0 _ = [ ]
take _ [ ] = [ ]
take n (x:xs) = x: take (n-1) xs --2 [8,9,4,5] = 8: take 1 [9,4,5] = [8,9]
--                                                      9: take 0 [4,5] = [9]
--                                                             [ ] 

drop :: Int -> [a] ->[a]
drop 0 xs = xs
drop _ [ ] = error "lista vazia"
drop n (x:xs) = drop (n-1) xs --vai removendo os primeiros itens da lista até que n seja 0, quando for, irá retornar uma lista com os itens que sobraram

takeWhile :: (a -> Bool) -> [a] -> [a] --recebe uma função(condição) que retorna um bool e uma lista, retornando outra lista com os elementos que satisfazeram a condição
takeWhile _ [ ] = [ ] --não importa a condição, se eu não tenho nada no que aplicar
takeWhile condicao (x:xs) 
 | condicao x = x: takeWhile condicao xs --se a condição der true no elemento x, x será adicionado ao prox resultado de takewhile com o restante da lista
 | otherwise  = [] --a condição aplicada em x deu errado, retorna uma lista vazia que pode ser juntada com outra, formando uma lista com algo dentro ou ser apenas vazia mesmo

dropWhile:: (a->Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile condicao (x:xs)
  | condicao x = dropWhile condicao xs--se deu true, eu continuo no restante da lista aplicando a condição
  |otherwise   = x:xs--deu false, a condição parou de ser aceita, ou seja, devo retornar a lista completa incluindo o 1° elemento que eu estava testando por isso x:xs e não apenas xs

tails::[a] ->[[a]] --recebe uma lista e retorna uam lista de listas :0
tails [ ] = [[]]
tails (x:xs) = (x:xs) : tails xs--tails [1,2,3] > [1,2,3] : tails [2,3] > [2,3] : tails [3] > [3] : tails[ ]
--                                                 [1,2,3],[2,3],[3],[] < [2,3],[3],[] < [3],[] 

-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

