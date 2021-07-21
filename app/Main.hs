module Main where

import Lib
import Data.Monoid
import qualified Control.Monad.State as S
import System.Random
import Control.Monad.Error
import Control.Applicative
isBigGang :: Int -> (Bool, String)
isBigGang = \x -> (x > 9, " is Big Gang")
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newlog) = f x in (y, log ++ newlog)
applyLogM :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLogM (x, log) f = let (y, newlog) = f x in (y, log `mappend` newlog)
type Food = String
type Price = Sum Int
addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whisky", Sum 100)
addDrink _ = ("beer", Sum 30)
newtype Writer w a = Writer { runWriter :: (a, w) }
instance (Monoid w) => Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)
instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, w) <*> Writer (a, w') = Writer (f a, w `mappend` w')
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (a, w)) >>= f = let (Writer (a', w')) = f a in Writer (a', w `mappend` w')
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got Number: " ++ show x])
tell :: [String] -> Writer [String] Int
tell w = Writer (1, w)

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a*b)
gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)
gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd'' b (a `mod` b)

-- difference list
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
instance Semigroup (DiffList a) where
  DiffList f <> DiffList g = DiffList (\xs -> f (g xs)) 
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
--fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result

tellp :: DiffList String -> Writer (DiffList String) Int
tellp w = Writer (1,  w)

gcdP :: Int -> Int -> Writer (DiffList String) Int
gcdP a b
  | b == 0  = do
      tellp (toDiffList ["Finished with " ++ show a])
      return a
  | otherwise = do
      result <- gcdP b (a `mod` b)
      tellp (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result
-- mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34
finalCountDown :: Int -> Writer (DiffList String) Int
finalCountDown 0 = do
  tellp (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x-1)
  tellp (toDiffList [show x])
finalCountDown' :: Int -> Writer [String] Int
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x-1)
  tell [show x]
newtype State s a = State { runState :: s -> (a,s) }
instance Functor (State s) where
  fmap f (State h) = State $ \s ->
    let (a, newState) = h s
    in (f a, newState)
instance Applicative (State s) where
  pure x = State (\s -> (x, s))
  (State f) <*> (State h) = State $ \s ->
    let (g, newState) = f s
        (a, newState') = h newState
    in (g a, newState')
instance Monad (State s) where
  return x = State (\s -> (x, s))
  (State f) >>= g = State $ \s -> let (a, s') = f s
                                      (State k) = g a
                                  in k s'
type Stack = [Int]
pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)
push :: Int -> State Stack ()
push a = State $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop
stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a==5
    then push 5
    else do
      push 3
      push 8
pop' :: S.State Stack Int
pop' = S.state $ \(x:xs) -> (x, xs)
push' :: Int -> S.State Stack ()
push' a = S.state $ \xs -> ((), a:xs)
stackManip' :: S.State Stack Int
stackManip' = do
  push' 3
  a <- pop'
  pop'
stackStuff' :: S.State Stack ()
stackStuff' = do
  a <- pop'
  if a == 5
    then push' 5
    else do
     push' 3
     push' 8
moreStack :: S.State Stack ()
moreStack = do
  a <- stackManip'
  if a == 100
    then stackStuff'
    else return ()
-- system random
randomSt :: (RandomGen g, Random a) => S.State g a
randomSt = S.state random
threeCoins :: S.State StdGen (Bool,Bool,Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)
keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
      tell ["Keeping " ++ show x]
      return True
  | otherwise = do
      tell [show x ++ " is too large, throwing it away"]
      return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x->[True, False]) xs
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)
f = foldr (.) id [(+1), (*100), (+3)]
f2 = foldl (.) id [(+1), (*100), (+3)]

--S.runState threeCoins (mkStdGen 33)

-- Error handling

main :: IO ()
main = mapM_ putStrLn $ snd $ runWriter (gcd'' 8 3)
--main = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000
--main = mapM_ putStrLn . snd . runWriter $ finalCountDown' 500000
