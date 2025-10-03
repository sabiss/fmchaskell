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

length :: Integral i => [a] -> i--integral faz com que 'i' passa ser um inteiro de precisão ilimitada
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a--Num faz com que coisas do tipo 'a' suportem operações matemáticas 
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

minimum :: Ord a => [a] -> a--Ord permite que coisas do tipo 'a' possam ser ordenadas, comparadas se são maior ou menor
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

take :: Int -> [a] -> [a]--Int faz 'a' ser um inteiro de precisão limitadax
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

init :: [a]->[a]--retorna todos os itens, menos o último
init [] = error "Lista Vazia"
init [a] = [ ]
init (x:xs) = x: init xs

inits :: [a] ->[[a]]
inits [] = [[]]
inits (x:xs) = [] : map(x:) (inits xs) 

-- subsequences

any:: (a->Bool) -> [a] -> Bool--verifica se tem PELO MENOS UM item que satisfaça uam condição
any _ [] = False --numa lista vazia não haverá um item sequer que satisfaça minha condição
any condicao (x:xs) 
  | condicao x =  True --achei o item que possue a condicao
  | otherwise = any condicao xs--não era o item x que safisfaz a condicao, continuo procurando

all::(a->Bool) ->[a]->Bool-- verifica se TODOS os itens dessa lista satisfazem a condição
all _ [] = True --se não tem um item na lista, não tem itens para NÃO-satisfazer a condicao, logo, é true
all condicao (x:xs) 
  | condicao x = all condicao xs--x satisfez a condicao, continue a verificar no restante da lista
  | otherwise = False--se um item não satisfez a condicao, então all dessa lista é false

and::[Bool] -> Bool--pega um array de booleanos e verifica se todos são true ou não
and [] = True
and (x:xs) 
  | x = and xs --o item x é True, então ele vai verificar o restante da lista
  | otherwise = False--um item é false, já joga um false na saida

or::[Bool] -> Bool
or [] = False --numa lista vazia não vai haver nada true ne
or (x:xs )
  |x = True--achei um caso válido, já satisfaz a "ou"
  |otherwise = or xs--não achei um caso true, continuo procurando

concat::[[a]]->[a]--recebe uma lista de listas e retorna uma lista com todos os itens juntos numa lista só
concat [] = [ ]
concat (x:xs) =  x ++ concat xs--EX: [[1,2],[3,4]] = [1,2] ++ concat [[3,4]] > concat [3,4] ++ []
--                                      [1,2,3,4]    [1,2] ++     [3,4]                  [3,4]

-- elem using the funciton 'any' above
elemComAny :: (a->Bool) -> [a] -> Bool
elemComAny = any

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

--Eq faz com coisa do tipo 'a' possam ser comparados
elemSemAny:: Eq a => a -> [a] -> Bool--elem verifica se PELO MENOS UM item está presente na lista ou não
elemSemAny _ [] = False
elemSemAny n (x:xs) =  if n == x then True else elemSemAny n xs

(!!)::Int->[b] -> b--recebo um index, uma lista de item e retorno o item correspondente aquele index
0 !! (x:_) = x--se o index for zero, é por que é o 1° item de uma lista qualquer que deve ser retornado
index !! (_:xs) = (index-1) !! xs--vou "retirando" itens da lista e diminuindo o index ate que chegue a zero, como, estou retirando tanto um item da lista com 1 do numerador, quando o index for zero, é pq cheguei no item que ele queria 
--[EX]: 2 !! [1,3,5,8], ele quer 5, então vou fazendo: 1 !! [3,5,8] > 0 !! [5,8], no index 0 o 1° item é justamente o 5 que queria

filter::(a->Bool)->[a]->[a]--recebe uma condicao a ser atendida, uma lista e retorna todos os itens da lista que satisfazeram a condicao
filter _ [ ] = []--independente da condicao, se eu não tenho nada pra testar então os itens que safisfazeram a condição foram nenhum(lista vazia)
filter condicao (x:xs) 
  |condicao x = x: filter condicao xs--se x satisfez a condicao pego 'x' e junto com o resultado da filtragem no restante da lista
  |otherwise = filter condicao xs--x não satisfez, então devo apenas continuar procurando um que satisfaça


map::(a->b) -> [a] -> [b]
map _ [] = []
map funcao (x:xs) = funcao x : map funcao xs

-- cycle

repeat:: a ->[a]--pega um item e cria uma lista infinita com esse item dentro
repeat item = item : repeat item

--Num a => a->b->[b]não daria certo, pois Num so me da acesso a operações matemáticas e não aos numeros em si
replicate::Int -> b->[b]--irá pegar um número, um item e devolver uam lista com esse item dentro repetido a quantidade de vezes dada
replicate 0 _ = []
replicate quantidade item = item: replicate (quantidade-1) item

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

zip::[a]->[b]->[(a,b)]--pega uma lista A e uma lista B e retorna os primeiros itens de cada um juntos numa tupla EX: [1,2] e [3,4] zipando elas teriamos: [(1,3),(2,4)]
zip _ [] = []
zip (x:xs) (y:ys) = 
  if length (x : xs) /= length (y : ys) then
    error "listas de tamanho difentes" --pra zipar obviamente tem que ter o mesmo tamanho pra cada item ter um par
  else
    (x, y) : zip xs ys

zipWith:: [a] -> [b] -> (a->b->c) -> [c]--pega os 1° itens de cada lista e aplica um função neles, depois cria uma lista com todos os resultados
zipWith _ [] _ = []
zipWith (x:xs) (y:ys) funcao = funcao x y : zipWith xs ys funcao

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

