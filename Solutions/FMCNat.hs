{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where
    (==) O O = True
    (==) _ O = False
    (==) O _ = False
    (==) (S n) (S m) = (==) n m--minha ideia é ir diminuindo 1 a 1, se os dois chegarem a zero juntos, é por que são iguais, se não, não são iguais

instance Ord Nat where
    (<=) O O = True
    (<=) O _ = True
    (<=) _ O = False
    (<=) (S n) (S m) = (<=) n m    

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O n = O
    min n O = O
    min (S n) (S m)= S(min n m)--min 5 6 = 1+min 4 5 > 1+min 3 4 > 1+min 2 3 > 1+min 1 2 > 1+min 0 1
    --                               5          5            4           3         2         0+1=1
    max O n = n
    max n O = n
    max (S n) (S m) = S(max n m) --técnica similar ao min


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S(S n)) = even n

odd :: Nat -> Bool
odd O = False --par
odd (S O) = True --impar 
odd (S(S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) n O   = n
(<+>) n (S m)  = S (n + m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O --se o primeiro número acabar antes do segundo, então ele é menor, logo, dará negativo e, pelo enunciado, deve ser zero
monus n O = n --5-0=5
monus (S n) (S m) = monus n m 

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times _ O = O --usa o 'n' quando precisar dele do outro lado da equação, se não usa o'_' mesmo
times n  (S m) = n + (n * m) -- 5*S(S O) = 5+(5*SO) > 5+SO = 5+(5*O)
                          --         10    5+5               5+0

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _  O = S O
pow n (S O) = n
pow n (S m) = n * pow n m--3*4 = 3*(4-1) > 3*(3-1) > 3*(2-1) > 3*(1)
--                         81     3*27      9*3=27       3*3=9   3

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) numerador (S O) = numerador
(</>) O _ = O
(</>) _ O = O
(</>) numerador denominador =
    case denominador-numerador of
      S _ -> O--d > m 8/10 quantas vezes 10 cabe inteiramente em 8? NENHUMA 
      O -> S O + (</>) (numerador-denominador) denominador --n>d

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) n O = n
(<%>) n m = 
    case n-m of
      S _ -> n - ((</>)n m * m) -- 6%3 = 6-((6/3)*3) 
      O -> n --se eu dividir 2/5, 5 não cabe em 2, a divisão é 0 e sobra 2

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (numerador, denominador) = (numerador </> denominador, numerador <%> denominador)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) _ O = undefined
(<|>) _ (S O) = True
(<|>) n m = 
  case (<%>) n m of
    O -> True
    S _ -> False

divides = (<|>)
-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n O = n
dist O n = n
dist n m = (n-m) + (m-n)

(|-|) = dist

factorial :: Nat -> Nat
factorial O = O 
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg (S n) = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O O = undefined
lo _ O = undefined
lo O _ = undefined
lo (S O) (S O) = undefined
lo (S O) _ = undefined
lo _ (S O) = O
lo n m =  
  if (<=) m n then O else S (lo n ((</>) m n))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n = S (toNat (n-1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

