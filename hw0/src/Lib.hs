{-# LANGUAGE TypeOperators #-}

module Lib ( distributivity
           , associator
           , eitherAssoc
           , doubleNeg
           , excludedNeg
           , peirce
           , doubleNegElim
           , thirdNegElim
           , composition
           , identity
           , contraction
           , permutation
           , iterateElement
           , fibonacci
           , factorial
           , mapFix
           , zero
           , succChurch
           , churchPlus
           , churchMult
           , churchToInt
           , fn1
           , fn2
           , fn3) where

import Data.Void (Void)
import Data.Function (fix)
import Data.Either (lefts, rights)

--Task 1
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eighthAxiom (Left . Left) (eighthAxiom (Left . Right) Right),
               eighthAxiom (eighthAxiom Left (Right . Left)) (Right . Right))
               where
                eighthAxiom l _ (Left a) = l a
                eighthAxiom _ r (Right b) = r b

--Task 2
type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg a f = f a

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = ninthAxiom (\f a -> f (Left a)) (\f a -> f (Right a))
              where
                ninthAxiom f g a = (g a) (f a)

peirce :: ((a -> b) -> a) -> a
peirce = undefined

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f a = f (doubleNeg a)

--Task 3
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k a _ = a

composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (k s) k

identity :: a -> a
identity = s k k

contraction :: (a -> a -> b) -> a -> b
contraction = s s (s k)

permutation :: (a -> b -> c) -> b -> a -> c
permutation = (s (s (k (s (k s) k)) s) (k k))

--Task 4
iterateElement :: a -> [a]
iterateElement el = fix (el :)

fibonacci :: Integer -> Integer
fibonacci = fix (\rec n -> if n <= 2
                           then 1
                           else rec (n - 2) + rec (n - 1))

factorial :: Integer -> Integer
factorial = fix (\rec n -> if n <= 1
                           then 1
                           else n * rec (n - 1))

mapFix :: (a -> b) -> [a] -> [b]
mapFix f = fix (\rec l -> case l of
                            [] -> []
                            (x : xs) -> f x : rec xs)

--Task 5
type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch = \n f x -> f (n f x)

churchPlus, churchMult :: Nat a -> Nat a -> Nat a
churchPlus = \a b f x -> a f (b f x)
churchMult = \a b f -> a (b f)

churchToInt :: Nat Integer -> Integer
churchToInt = \a -> a (+ 1) 0

--Task 6
-- distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
-- Outermost is distributivity f, apply it
-- (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))
-- Outermost is a constructor "," -> WHNF

-- null $ mapMaybe foo "pole chudes ochen' chudesno"
-- foo :: Char -> Maybe Double
-- foo char =
--    case char == 'o' of
--      True -> Just $ exp pi
--      False -> Nothing
-- Outermost is null l, apply it
-- foldr (\_ _ -> False) True (mapMaybe foo "pole chudes ochen' chudesno")
-- Outermost is applying foldr; apply all args. Now we need to evaluate the first element of mapMaybe to patternmatch against it
-- foldr (\_ _ -> False) True ((Just $ exp pi) : (mapMaybe foo "le chudes ochen' chudesno"))
-- (\_ _ -> False) (Just $ exp pi) (foldr (\_ _ -> False) True (mapMaybe foo "le chudes ochen' chudesno"))
-- Outermost is applying the first lambda; apply all args
-- False -> WHNF

--Task 7
fn1 :: Bool
fn1 = (((($) :: ([[Char]] -> Bool) -> [[Char]] -> Bool)
      f1) :: [[Char]] -> Bool) f2
      where
        f1 :: [[Char]] -> Bool
        f1 = (((.) :: ([Char] -> Bool) -> ([[Char]] -> [Char]) -> [[Char]] -> Bool)
              (null :: [Char] -> Bool)
              :: ([[Char]] -> [Char]) -> [[Char]] -> Bool)
              (head :: [[Char]] -> [Char])
        f2 :: [[Char]]
        f2 = (((map :: (([Char] -> [Char], [Char]) -> [Char]) -> [([Char] -> [Char], [Char])] -> [[Char]])
             f3) :: [([Char] -> [Char], [Char])] -> [[Char]]) f4
             where
                f3 :: ([Char] -> [Char], [Char]) -> [Char]
                f3 = (uncurry :: (([Char] -> [Char]) -> [Char] -> [Char]) -> ([Char] -> [Char], [Char]) -> [Char])
                     (id :: ([Char] -> [Char]) -> ([Char] -> [Char]))
                f4 :: [([Char] -> [Char], [Char])]
                f4 = [f5]
                  where
                    f5 :: ([Char] -> [Char], [Char])
                    f5 = ((((,) :: ([Char] -> [Char]) -> [Char] -> ([Char] -> [Char], [Char]))
                      f6) :: [Char] -> ([Char] -> [Char], [Char])) f7
                      where
                        f6 :: [Char] -> [Char]
                        f6 = ((++) :: [Char] -> [Char] -> [Char])
                              ("Dorian " :: [Char])
                        f7 :: [Char]
                        f7 = " Grey"

fn2 :: [(Integer, Integer)]
fn2 = f1 f2
      where
        f1 :: [Either Integer Integer] -> [(Integer, Integer)]
        f1 x = f3
          where
          f3 :: [(Integer, Integer)]
          f3 = (((zip :: [Integer] -> [Integer] -> [(Integer, Integer)])
                f4) :: [Integer] -> [(Integer, Integer)]) f5
                where
                    f4 :: [Integer]
                    f4 = (lefts :: [Either Integer Integer] -> [Integer])
                         (x :: [Either Integer Integer])
                    f5 :: [Integer]
                    f5 = (rights :: [Either Integer Integer] -> [Integer])
                         (x :: [Either Integer Integer])
        f2 :: [Either Integer Integer]
        f2 = [f6, f7]
          where
            f6 :: Either Integer Integer
            f6 = (Left :: Integer -> Either Integer Integer)
                 f8
                 where
                  f8 :: Integer
                  f8 = ((((+) :: Integer -> Integer -> Integer)
                       (1 :: Integer)) :: Integer -> Integer) (2 :: Integer)
            f7 :: Either Integer Integer
            f7 = (Right :: Integer -> Either Integer Integer)
                 f9
                 where
                  f9 :: Integer
                  f9 = ((((^) :: Integer -> Integer -> Integer)
                       (2 :: Integer)) :: Integer -> Integer) (6 :: Integer)

fn3 :: Integer -> Bool
fn3 z = let impl :: Bool -> Bool -> Bool
            impl x y = (((((||) :: Bool -> Bool -> Bool)
                        t1) :: (Bool -> Bool)) t2) :: Bool
                       where
                          t1 :: Bool
                          t1 = (not :: Bool -> Bool)
                               (x :: Bool)
                          t2 :: Bool
                          t2 = y
          in
          let isMod2 :: Integer -> Bool
              isMod2 x = (((((==) :: Integer -> Integer -> Bool)
                        i1) :: (Integer -> Bool)) i2) :: Bool
                        where
                           i1 :: Integer
                           i1 = (((mod :: Integer -> Integer -> Integer)
                                 (x :: Integer)) :: Integer -> Integer) (2 :: Integer)
                           i2 :: Integer
                           i2 = 0
          in
          let isMod4 :: Integer -> Bool
              isMod4 x = (((((==) :: Integer -> Integer -> Bool)
                        i1) :: (Integer -> Bool)) i2) :: Bool
                        where
                           i1 :: Integer
                           i1 = (((mod :: Integer -> Integer -> Integer)
                                 (x :: Integer)) :: Integer -> Integer) (4 :: Integer)
                           i2 :: Integer
                           i2 = 0
          in ((impl ((isMod4 z) :: Bool) :: Bool -> Bool)) ((isMod2 z) :: Bool)
