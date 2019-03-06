{-# LANGUAGE InstanceSigs #-}
module Lib ( order3
           , smartReplicate
           , contains
           , stringSum
           , (!!!)
           , mergeSort
           , Day
           , nextDay
           , afterDays
           , isWeekend
           , daysToParty
           , House
           , NonEmpty
           , Houses
           , Wall
           , Lord
           , Castle
           , ChurchOrLibrary
           , City
           , buildCastle
           , buildChurch
           , buildLibrary
           , buildWalls
           , buildHouse
           , addLord
           , Nat
           , natToInteger
           , isEven
           , divNat
           , remainderNat
           , Tree
           , isEmptyTree
           , treeSize
           , containsTree
           , insertTree
           , fromList
           , deleteFromTree
           , Pair
           , splitOn
           , joinWith
           , maybeConcat
           , eitherConcat
           , fromString
           , toString) where

import Data.List (sort, genericReplicate)

--Task 1
order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) = let x:y:z:_ = sort ([a, b, c]) in (x, y, z)

smartReplicate :: Integral i => [i] -> [i]
smartReplicate [] = []
smartReplicate (x:xs) = genericReplicate x x ++ (smartReplicate xs)
                
contains :: Eq a => a -> [[a]] -> [[a]]
contains a l = filter (a `elem`) l

stringSum :: String -> Int
stringSum s = sum (map read (words s))

--Task 2
(!!!) :: [a] -> Int -> (a, [a])
(!!!) [] _ = error "!!!: empty list"
(!!!) l n
  | n < 0 = error "!!!: negative index"
  | n > (length l) = error "!!!: index out of bounds"
  | otherwise = (l !! n, take n l ++ (drop (n + 1) l))

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort first) (mergeSort second)
              where
                merge x [] = x
                merge [] y = y
                merge a@(x:xs) b@(y:ys) = if (x < y)
                                          then x : (merge xs b)
                                          else y : (merge a ys)
                (first, second) = splitAt ((length l) `div` 2) l

--Task 3
data Day = Monday 
         | Tuesday 
         | Wednesday 
         | Thursday 
         | Friday 
         | Saturday 
         | Sunday
         deriving Show

nextDay :: Day -> Day
nextDay d = case d of
              Monday -> Tuesday
              Tuesday -> Wednesday
              Wednesday -> Thursday
              Thursday -> Friday
              Friday -> Saturday
              Saturday -> Sunday
              Sunday -> Monday

afterDays :: Day -> Int -> Day
afterDays d n
  | n < 0 = error "afterDays: negative argument"
  | otherwise = let it = (n `mod` 7) in (iterate nextDay d) !! it

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty :: Day -> Int
daysToParty d = (dayToNumber Friday - dayToNumber d) `mod` 7
                where
                  dayToNumber x = case x of
                                    Monday -> 1
                                    Tuesday -> 2
                                    Wednesday -> 3
                                    Thursday -> 4
                                    Friday -> 5
                                    Saturday -> 6
                                    Sunday -> 7


data House = HOne
           | HTwo
           | HThree
           | HFour
           deriving Show

data NonEmpty a = a :| [a] deriving Show

data Houses = Houses (NonEmpty House) deriving Show

data Wall = Wall deriving Show

data Lord = Lord deriving Show

data Castle = Castle (Maybe Wall) (Maybe Lord) deriving Show

data ChurchOrLibrary = Church | Library deriving Show

data City = City { castle :: Maybe Castle
                 , churchOrLibrary :: Maybe ChurchOrLibrary
                 , houses :: Houses
                 } deriving Show

buildCastle :: City -> (City, Bool)
buildCastle c@City {castle = Nothing} = (c {castle = Just (Castle Nothing Nothing)}, True)
buildCastle c = (c, False)

buildChurch :: City -> (City, Bool)
buildChurch c@City {churchOrLibrary = Nothing} = (c {churchOrLibrary = Just Church}, True)
buildChurch c = (c, False)

buildLibrary :: City -> (City, Bool)
buildLibrary c@City {churchOrLibrary = Nothing} = (c {churchOrLibrary = Just Library}, True)
buildLibrary c = (c, False)

buildHouse :: City -> Int -> City
buildHouse c@City {houses = Houses (h :| xh)} n = c {houses = Houses (h :| ((intToHouse n) : xh))}
                                                  where
                                                    intToHouse x = case x of
                                                                    1 -> HOne
                                                                    2 -> HTwo
                                                                    3 -> HThree
                                                                    4 -> HFour
                                                                    _ -> error "Can't construct a house for such a number"

addLord :: City -> City
addLord City {castle = Nothing} = error "Can't add a lord if there's no castle"
addLord City {castle = Just (Castle _ (Just Lord))} = error "Can't add a lord if there's already a lord"
addLord c@City {castle = Just (Castle w Nothing)} = c {castle = Just (Castle w (Just Lord))}

getPopulation :: Houses -> Int
getPopulation (Houses (h :| xh)) = houseToInt h + sum (map houseToInt xh)
                                   where
                                    houseToInt n = case n of
                                                    HOne -> 1
                                                    HTwo -> 2
                                                    HThree -> 3
                                                    HFour -> 4

buildWalls :: City -> City
buildWalls City {castle = Nothing} = error "Can't build walls if there is no castle"
buildWalls City {castle = Just (Castle _ Nothing)} = error "Can't build walls if there is no lord"
buildWalls City {castle = Just (Castle (Just Wall) _)} = error "The walls are built already"
buildWalls c@City {castle = Just (Castle Nothing (Just Lord)), houses = h} =  if ((getPopulation h) < 10)
                                                                              then error "Can't build walls if there are less than 10 people in the city"
                                                                              else c {castle = Just (Castle (Just Wall) (Just Lord))}


data Nat = Z
         | S Nat
         deriving Show

instance Num Nat where
  (+) a Z = a
  (+) a (S b) = (S a) + b

  (-) a Z = a
  (-) Z _ = Z
  (-) (S a) (S b) = a - b

  (*) _ Z = Z
  (*) a (S b) = a + (a * b)

  negate _ = Z

  abs = id

  signum Z = Z
  signum _ = S Z

  fromInteger n
          | n <= 0 = Z
          | otherwise = S (fromInteger (n - 1))

natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (S k) = 1 + natToInteger k

instance Eq Nat where
  Z == Z = True
  (S a) == (S b) = a == b
  _ == _ = False

instance Ord Nat where
  Z <= _ = True
  (S a) <= (S b) = a <= b
  _ <= _ = False

isEven :: Nat -> Bool
isEven Z = True
isEven (S Z) = False
isEven (S (S a)) = isEven a

divNat :: Nat -> Nat -> Nat
divNat a b = if (a < b)
             then 0
             else 1 + divNat (a - b) b

remainderNat :: Nat -> Nat -> Nat
remainderNat a b = if (a < b)
                   then a
                   else remainderNat (a - b) b


data Tree a = Leaf
            | Node { list :: NonEmpty a
                   , left :: Tree a
                   , right :: Tree a
                   } deriving Show

isEmptyTree :: Tree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _ = False

treeSize :: Tree a -> Int
treeSize Leaf = 0
treeSize (Node (_ :| xs) l r) = 1 + length xs + (treeSize l) + (treeSize r)

containsTree :: Ord a => Tree a -> a -> Bool
containsTree Leaf _ = False
containsTree (Node (x :| _) l r) el = case (compare el x) of
                                        EQ -> True
                                        LT -> containsTree l el
                                        GT -> containsTree r el

insertTree :: Ord a => Tree a -> a -> Tree a
insertTree Leaf a = Node (a :| []) Leaf Leaf
insertTree n@(Node (x :| xs) l r) el = case (compare el x) of
                                        EQ -> n {list = (x :| (el : xs))}
                                        LT -> n {left = insertTree l el}
                                        GT -> n {right = insertTree r el}

fromList :: Ord a => [a] -> Tree a
fromList [] = Leaf
fromList (x : xs) = insertTree (fromList xs) x

removeNextAndReplace :: Tree a -> Tree a
removeNextAndReplace Leaf = Leaf
removeNextAndReplace Node {left = l, right = Leaf} = l
removeNextAndReplace n@Node {right = r} = n {list = newList, right = newRight}
                                          where
                                            removeLeftmostAndReturnList Leaf = error "removeLeftmostAndReturnList cannot be applied to Leaf"
                                            removeLeftmostAndReturnList Node {left = Leaf, right = r1, list = l} = (l, r1)
                                            removeLeftmostAndReturnList p@Node {left = l} = (f, p {left = s})
                                              where
                                                (f, s) = removeLeftmostAndReturnList l
                                            (newList, newRight) = removeLeftmostAndReturnList r

deleteFromTree :: Ord a => Tree a -> a -> Tree a
deleteFromTree Leaf _ = Leaf
deleteFromTree n@(Node (x :| xs) l r) el = case (compare el x) of
                                            EQ -> case xs of
                                                    [] -> removeNextAndReplace n
                                                    (y : ys) -> n {list = y :| ys}
                                            LT -> n {left = deleteFromTree l el}
                                            GT -> n {right = deleteFromTree r el}

--Task 4
data Pair a = Pair a a

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair a1 a2) = (f a1) <> (f a2)

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair a1 a2) = f a1 (f a2 z)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = (f x) <> (foldMapList f xs)
                        where
                          foldMapList _ [] = mempty
                          foldMapList fun (y : ys) = (fun y) <> (foldMapList fun ys)

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldrList f z xs)
                        where
                          foldrList _ el [] = el
                          foldrList fun el (y : ys) = fun y (foldrList fun el ys)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node nel l r) = (foldMap f nel) <> (foldMap f l) <> (foldMap f r)

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Node nel l r) = foldr f (foldr f (foldr f z r) nel) l

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn el l = foldr splitList ([] :| []) l
               where
                  splitList cur (f :| s)
                    | cur == el = [] :| (f : s)
                    | otherwise = (cur : f) :| s

joinWith :: a -> NonEmpty [a] -> [a]
joinWith el ll = foldr1 joinList ll
                 where
                   joinList cur acc = cur ++ (el : acc)

--Task 5
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat l = foldr concatLists [] l
                where
                  concatLists (Just li) acc = li ++ acc
                  concatLists _ acc = acc

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat l = foldr concatEithers (mempty, mempty) l
                 where
                   concatEithers (Left a) (x, y) = (a <> x, y)
                   concatEithers (Right b) (x, y) = (x, b <> y)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))

data ThisOrThat a b = This a
                    | That b
                    | Both a b
                    deriving Show

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) c@(Both _ _) _ = c
  (<>) (This a) (That b) = Both a b
  (<>) (That b) (This a) = Both a b
  (<>) (This a1) (Both _ b2) = Both a1 b2
  (<>) (That b1) (Both a2 _) = Both a2 b1
  (<>) a _ = a

data Name = Name String deriving Show

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) (Name "") s = s
  (<>) f (Name "") = f
  (<>) (Name s1) (Name s2) = Name (s1 ++ "." ++ s2)

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id

data Builder = One Char | Many [Builder] deriving Show

instance Semigroup Builder where
  (<>) :: Builder -> Builder -> Builder
  (<>) a b = Many (getList (lift a) ++ getList (lift b))
             where
               getList (Many bd) = bd
               getList _ = []
               lift bd@(One _) = Many [bd]
               lift bd = bd

fromString :: String -> Builder
fromString s = Many (map (\c -> One c) s)

toString :: Builder -> String
toString (One c) = [c]
toString (Many l) = concat (map toString l)
