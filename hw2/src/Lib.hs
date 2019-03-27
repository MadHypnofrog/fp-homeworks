module Lib
    ( stringSum
    , Expr (..)
    , ArithmeticError (..)
    , eval
    , moving
    , MonadFish (..)
    , MonadJoin (..)
    , Parser (..)
    , ok
    , eof
    , satisfy
    , element
    , stream
    , parseNTimes
    , parenthesesParser
    , numberParser
    , sepBy
    , listlistParser
    ) where

import Control.Applicative ( Alternative (empty, (<|>), some, many)
                           , liftA2
                           )
import Control.Arrow (first)
import Control.Monad.State ( State
                           , get
                           , replicateM
                           , runState
                           , state
                           )
import Data.Char ( isDigit
                 , isSeparator
                 )
import Text.Read (readMaybe)

--Task 1
stringSum :: String -> Maybe Int
stringSum s = fmap sum (traverse readMaybe (words s))

data Tree a 
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure a = Leaf a
  (<*>) funTree valTree = case funTree of
                            Leaf f -> case valTree of
                                              Leaf a -> Leaf (f a)
                                              Branch l r -> Branch (applyFun f l) (applyFun f r)
                            Branch lf rf -> case valTree of
                                              Leaf a -> Branch (applyVal lf a) (applyVal rf a)
                                              Branch lv rv -> Branch (lf <*> lv) (rf <*> rv) 
                          where
                            applyFun f (Leaf a) = Leaf (f a)
                            applyFun f (Branch lv rv) = Branch (applyFun f lv) (applyFun f rv)
                            applyVal (Leaf f) a = Leaf (f a)
                            applyVal (Branch lf rf) a = Branch (applyVal lf a) (applyVal rf a)

instance Foldable Tree where
  foldr f z (Leaf a) = f a z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

data NonEmpty a = a :| [a] deriving Show

instance Functor NonEmpty where
  fmap f (a :| l) = f a :| (fmap f l)

instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) (f :| fx) (v :| vx) = (f v) :| applyPair f v fx vx
    where
      applyPair f1 _ [] v1x = map f1 v1x
      applyPair _ v1 f1x [] = map ($ v1) f1x
      applyPair _ _ (f1 : f1x) (v1 : v1x) = (f1 v1) : applyPair f1 v1 f1x v1x

instance Monad NonEmpty where
  (>>=) (a :| l) f = x :| (xs ++ ls)
    where
      (x :| xs) = f a
      ls = l >>= (\b -> let (xl :| xls) = f b in xl : xls)

instance Foldable NonEmpty where
  foldr f z (a :| l) = f a (foldr f z l)

instance Traversable NonEmpty where
  traverse f (a :| l) = (:|) <$> (f a) <*> traverse f l

--Task 2
data Expr 
  = Sum Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Const Int
  deriving Show

data ArithmeticError
  = DivisionByZero
  | NegativePower
  deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval e = case e of
           Sum e1 e2 -> doAction (+) (eval e1) (eval e2)
           Sub e1 e2 -> doAction (-) (eval e1) (eval e2)
           Mul e1 e2 -> doAction (*) (eval e1) (eval e2)
           Div e1 e2 -> let d = eval e2 in
                          case d of
                            Left err -> Left err
                            Right num -> if (num == 0)
                                        then Left DivisionByZero
                                        else doAction div (eval e1) d
           Pow e1 e2 -> let p = eval e2 in
                          case p of
                            Left err -> Left err
                            Right num -> if (num < 0)
                                        then Left NegativePower
                                        else doAction (^) (eval e1) p
           Const i -> Right i
         where
           doAction f e1 e2 = liftA2 f e1 e2

moving :: Fractional a => Int -> [a] -> [a]
moving n l = fst $ runState (replicateM (length l) genState) ([], l, 0)
  where
    genState :: Fractional a => State ([a], [a], a) a
    genState = do
      (window, _, _) <- get
      if (length window == n)
        then
          popFromWindow
        else 
          return ()
      pushToWindow
      computeAverage
    popFromWindow :: Fractional a => State ([a], [a], a) ()
    popFromWindow = state $ \(x : xw, li, s) -> ((), (xw, li, (s - x)))
    pushToWindow :: Fractional a => State ([a], [a], a) ()
    pushToWindow = state $ \(w, x : xl, s) -> ((), ((w ++ [x]), xl, (s + x)))
    computeAverage :: Fractional a => State ([a], [a], a) a
    computeAverage = state $ \st@(w, _, s) -> (s / (fromIntegral (length w)), st)

--Monad is defined in MonadDecl to avoid conflicts
--Instances are in MonadFish and MonadJoin

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a

--Task 3
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f (Parser g) = Parser $ fmap (first f) . g

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) (Parser fa) (Parser a) = Parser $ \s -> case (fa s) of
                                         Nothing -> Nothing
                                         Just (f, l) -> (a l) >>= Just . first f

instance Monad (Parser s) where
  (>>=) (Parser fa) f = Parser $ \s -> case (fa s) of
                                        Nothing -> Nothing
                                        Just (a, l) -> runParser (f a) l

instance Alternative (Parser s) where
  empty = Parser (\_ -> Nothing)
  (<|>) (Parser a) (Parser b) = Parser $ \s -> (a s) <|> (b s)

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
                       [] -> Just ((), [])
                       _ -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s -> case s of
                             [] -> Nothing
                             x : xs -> if (p x)
                                       then Just (x, xs)
                                       else Nothing

element :: Eq s => s -> Parser s s
element el = satisfy (el ==)

stream :: Eq s => [s] -> Parser s [s]
stream l = traverse element l

parseNTimes :: Int -> Parser s a -> Parser s [a]
parseNTimes 0 _ = pure []
parseNTimes n p = p >>= \val -> parseNTimes (n - 1) p >>= \rest -> pure (val : rest)

parenthesesParser :: Parser Char ()
parenthesesParser = parse >> eof
  where
    parse = ((element '(') >> parse >> (element ')') >> parse) <|> ok

numberParser :: Parser Char Int
numberParser = parse
  where
    parse = ((element '+') <|> (element '-') <|> pure '+') >>= \sign -> some (satisfy isDigit) >>= \n -> if (sign == '+')
                                                                                                         then pure (read n)
                                                                                                         else pure ((read n) * (-1))

sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy p sep = ((:) <$> p <*> (many (sep *> p))) <|> pure []

between :: Parser s a -> Parser s b -> Parser s a
between p sep = sep *> p <* sep

listlistParser :: Parser Char [[Int]]
listlistParser = between parse parseWhitespace >>= \res -> eof >> pure res
  where
    parse = sepBy parseList parseComma
    parseList = numberParser >>= \num -> if (num < 0)
                                         then empty
                                         else parseNTimes num (parseComma >> numberParser)
    parseComma = parseWhitespace >> element ',' >> parseWhitespace
    parseWhitespace = many (satisfy isSeparator)
