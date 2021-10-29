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
import Data.Vector as V (Vector, modify)
import Data.Vector.Generic.Mutable (write)

-- | @Error@ is the error type - useful for designating when something can error, and
-- bubbling it up
type Error a = Either String a

-- | @maybeToEither@ converts @Nothing@ into @Left a@ and @Just b@ into @Right b@
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither s Nothing = Left s
maybeToEither _ (Just a) = Right a

-- | @lookupEither@ looks up key @k@ in the @Map k v@, returning @Right v@ if the key is
-- found and returning @Left k@ if the key is not found
lookupEither :: (Ord k) => k -> Map k v -> Either k v
lookupEither k = lookupEither' k k

-- | @lookupEither'@ looks up key @k@ in the @Map k v@, returning @Right v@ if the key is
-- found and returning @Left s@ if the key is not found
lookupEither' :: (Ord k) => s -> k -> Map k v -> Either s v
lookupEither' s k = maybeToEither s . M.lookup k

-- | @fromJust@ safely extracts an @a@ from @Just a@, or returns the given @a@ if
-- @Nothing@ is given
fromJust :: a -> Maybe a -> a
fromJust _ (Just a) = a
fromJust a Nothing = a

-- | @updateVector@ updates the index of the given vector with the given value.
-- Recommended to use @Data.Vector.(//)@ instead if possible
updateVector :: Int -> a -> Vector a -> Error (Vector a)
updateVector i a v
  | i < 0 || i >= length v = Left "Index requested for update out of bounds"
  | otherwise = return $ modify (\v' -> write v' i a) v

-- | @dropNothings@ goes through a list of @Maybe@ values, and removes any @Nothing@s,
-- leaving only @a@s
dropNothings :: [Maybe a] -> [a]
dropNothings [] = []
dropNothings (Just a : xs) = a : dropNothings xs
dropNothings (Nothing : xs) = dropNothings xs

-- | @Single@ is a simple container for singular values
newtype Single a = Single a deriving (Show, Eq, Ord)

-- | @Peekable@ is a type class which designates whether a container type can be peeked
-- into, as well as swapping the first value.
--
-- @peek (swapFirst a as) == peek as >> return a@
class Peekable a where
  -- | @peek@ gets the first item in the type, and errors otherwise
  peek :: a b -> Error b

  -- | @swapFirst@ swaps the first item in the type for the given value
  swapFirst :: b -> a b -> a b

instance Peekable [] where
  peek [] = Left "Empty List when peeking"
  peek (x : _) = return x
  swapFirst _ [] = []
  swapFirst a (_ : as) = a : as

-- TODO: https://archives.haskell.org/projects.haskell.org/diagrams/tutorials.html
-- -- or https://discordapp.com/channels/195989586260918272/222003210670440451/709816458921640057

-- TODO: datatype for errors so more data can be revealed
