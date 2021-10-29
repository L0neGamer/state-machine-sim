module StateMachineSim.Lib.Lib
  ( Error,
    lookupEither,
    lookupEither',
    updateVector,
    dropNothings,
  )
where

import Data.Either.Extra (maybeToEither)
import Data.Map as M (Map, lookup)
import Data.Vector as V (Vector, modify)
import Data.Vector.Generic.Mutable (write)

-- | @Error@ is the error type - useful for designating when something can error, and
-- bubbling it up
type Error a = Either String a

-- | @lookupEither@ looks up key @k@ in the @Map k v@, returning @Right v@ if the key is
-- found and returning @Left k@ if the key is not found
lookupEither :: (Ord k) => k -> Map k v -> Either k v
lookupEither k = lookupEither' k k

-- | @lookupEither'@ looks up key @k@ in the @Map k v@, returning @Right v@ if the key is
-- found and returning @Left s@ if the key is not found
lookupEither' :: (Ord k) => s -> k -> Map k v -> Either s v
lookupEither' s k = maybeToEither s . M.lookup k

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

-- TODO: https://archives.haskell.org/projects.haskell.org/diagrams/tutorials.html
-- -- or https://discordapp.com/channels/195989586260918272/222003210670440451/709816458921640057

-- TODO: datatype for errors so more data can be revealed
