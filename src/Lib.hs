module Lib
  ( maybeToEither,
    Error,
    Single (..),
    lookupEither,
    lookupEither',
    updateVector,
    Peekable (..),
    fromJust,
    dropNothings,
  )
where

import Data.Map as M (Map, lookup)
import Data.Vector as V (Vector, (//))

type Error a = Either String a

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither s Nothing = Left s
maybeToEither _ (Just a) = Right a

lookupEither :: (Ord k) => k -> Map k v -> Either k v
lookupEither k = lookupEither' k k

lookupEither' :: (Ord k) => s -> k -> Map k v -> Either s v
lookupEither' s k = maybeToEither s . M.lookup k

fromJust :: a -> Maybe a -> a
fromJust _ (Just a) = a
fromJust a Nothing = a

updateVector :: Int -> a -> Vector a -> Maybe (Vector a)
updateVector i a v
  | i < 0 || i >= length v = Nothing
  | otherwise = Just $ v // [(i, a)]

dropNothings :: [Maybe a] -> [a]
dropNothings [] = []
dropNothings (Just a : xs) = a : dropNothings xs
dropNothings (Nothing : xs) = dropNothings xs

newtype Single a = Single a deriving (Show, Eq, Ord)

class Peekable a where
  peek :: a b -> Error b
  swapFirst :: b -> a b -> a b

instance Peekable [] where
  peek [] = Left "Empty List when peeking"
  peek (x : _) = return x
  swapFirst _ [] = []
  swapFirst a (_ : as) = a : as

-- TODO: https://archives.haskell.org/projects.haskell.org/diagrams/tutorials.html
-- -- or https://discordapp.com/channels/195989586260918272/222003210670440451/709816458921640057

-- TODO: datatype for errors so more data can be revealed

-- class StateMachine sm where
--   removeTransition :: (Ord a) => Transition a -> sm a -> sm a

--   reachableStates :: (Ord a) => sm a -> State -> States

--   isAcceptable :: (Ord a) => sm a -> State -> Bool
--   isAcceptable sm s = or $ S.map (`S.member` smAcceptStates sm) (reachableStates sm s)
