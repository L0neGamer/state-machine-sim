module Lib where

import Data.Map as M (Map, lookup, member, (!))
import Data.Set as S (Set, fromList, toList)
import Data.Vector as V (Vector, (//))

type Error a = Either String a

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither s Nothing = Left s
maybeToEither _ (Just a) = Right a

maybeToError :: String -> Maybe a -> Error a
maybeToError = maybeToEither

lookupEither :: (Ord k) => k -> Map k v -> Either k v
lookupEither k m
  | k `M.member` m = Right $ m M.! k
  | otherwise = Left k

lookupError :: (Ord k) => String -> k -> Map k v -> Error v
lookupError s k = maybeToError s . M.lookup k

updateVector :: Int -> a -> Vector a -> Maybe (Vector a)
updateVector i a v
  | i < 0 || i >= length v = Nothing
  | otherwise = Just $ v // [(i, a)]

mapMSet :: (Monad m, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapMSet f s = do
  l <- mapM f $ S.toList s
  return $ S.fromList l

dropNothings :: [Maybe a] -> [a]
dropNothings [] = []
dropNothings (Just a : xs) = a : dropNothings xs
dropNothings (Nothing : xs) = dropNothings xs

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

newtype Single a = Single a deriving (Show, Eq, Ord)

class Peekable a where
  peek :: a b -> Error b

instance Peekable [] where
  peek [] = Left "Empty List when peeking"
  peek (x : _) = Right x

-- -- TODO: https://archives.haskell.org/projects.haskell.org/diagrams/tutorials.html
-- -- or https://discordapp.com/channels/195989586260918272/222003210670440451/709816458921640057

-- class StateMachine sm where
--   removeTransition :: (Ord a) => Transition a -> sm a -> sm a

--   reachableStates :: (Ord a) => sm a -> State -> States

--   isAcceptable :: (Ord a) => sm a -> State -> Bool
--   isAcceptable sm s = or $ S.map (`S.member` smAcceptStates sm) (reachableStates sm s)
