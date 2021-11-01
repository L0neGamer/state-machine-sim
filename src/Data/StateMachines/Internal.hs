-- |
-- Module      :  Data.StateMachines.Internal
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- Utility functions that are needed but aren't strictly relevant to the module as a
-- whole.
module Data.StateMachines.Internal
  ( Error,
    lookupEither,
    lookupEither',
    updateVector,
  )
where

import Data.Either.Extra (maybeToEither)
import Data.Map as M (Map, lookup)
import Data.Vector as V (Vector, modify)
import Data.Vector.Generic.Mutable (write)

-- | The error type - useful for designating when something can error, and bubbling it up.
type Error a = Either String a

-- | Looks up key @k@ in the @Map k v@, returning @Right v@ if the key is found and
-- returning @Left k@ if the key is not found.
lookupEither :: (Ord k) => k -> Map k v -> Either k v
lookupEither k = lookupEither' k k

-- | Looks up key @k@ in the @Map k v@, returning @Right v@ if the key is found and
-- returning @Left s@ if the key is not found.
lookupEither' :: (Ord k) => s -> k -> Map k v -> Either s v
lookupEither' s k = maybeToEither s . M.lookup k

-- | Updates the index of the given vector with the given value.
--
-- Recommended to use `Data.Vector.(//)` instead if possible.
updateVector :: Int -> a -> Vector a -> Error (Vector a)
updateVector i a v
  | i < 0 || i >= length v = Left "Index requested for update out of bounds"
  | otherwise = return $ modify (\v' -> write v' i a) v

-- TODO: https://archives.haskell.org/projects.haskell.org/diagrams/tutorials.html
-- -- or https://discordapp.com/channels/195989586260918272/222003210670440451/709816458921640057

-- TODO: datatype for errors so more data can be revealed
