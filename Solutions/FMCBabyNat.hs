module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight, nine, ten :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine = S eight
ten = S nine

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O --sim
isZero (S n) = O --não

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O 
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat 
even O = S O --par
even (S O) = O -- impar
even (S(S n)) = even n
-- dois casos base, se for zero=par, se for um=impar. S(S n) eu vou olhando os sucessores de dois em dois. 
-- EX: S(S(S O)) -> even S O = impar, pego o número, diminuo 2 dele e recoloco o número resultante em EVEN até atingir um caso base

odd :: Nat -> Nat
odd O = O --false = par
odd (S O) = S O --true = impar 
odd (S(S n)) = odd n
-- 1 3 5 7 9 os impares vão acontecendo de trás pra frente de -2 em -2 até chegar no caso base 1



-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O n = O --se o primeiro número acabar antes do segundo, então ele é menor, logo, dará negativo e, pelo enunciado, deve ser zero
monus n O = n --5-0=5
monus (S n) (S m) = monus n m -- vai diminuindo de um em um os parâmetros até chegar num caso base

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
_ * O = O --usa o 'n' quando precisar dele do outro lado da equação, se não usa o'_' mesmo
n * (S m) = n + (n * m) -- 5*S(S O) = 5+(5*SO) > 5+SO = 5+(5*O)
                          --    10    5+5               5+0
infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ (S O) = n
n ^ (S m) = n * (n ^ m)--3*4 = 3*(4-1) > 3*(3-1) > 3*(2-1) > 3*(1)
--                       81     3*27      9*3=27       3*3=9   3


-- decide: infix? ? ^

-- quotient
(/) :: Nat -> Nat -> Nat
numerador / S O = numerador
O / _ = O
_ / O = O
numerador / denominador =
    case denominador-*numerador of
      S _ -> O--d > m 8/10 quantas vezes 10 cabe inteiramente em 8? NENHUMA 
      O -> S O + ((numerador-*denominador)/denominador) --n>d
      
infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
_ % S O = O
O % _ = O
S n % S m = n%m
infixl 7 %

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) = undefined

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = O --apenas para não explodir
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat --um natural nunca será negativo, então so tenho que tratar quando for zero ou um 
sg O = zero
sg (S n) = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

