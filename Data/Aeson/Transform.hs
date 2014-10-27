module Data.Aeson.Transform
  (
    -- * Example usage
    -- $use

    -- * Transformations
    Builder
  , at
  , attr
  , keep
  , Data.Aeson.Transform.map
  , Data.Aeson.Transform.index
  , atIndex
  , obj
  , merge
  ) where

import Data.Aeson as A
import Data.Text
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as H

type Builder = Value -> Value

-- | Move to value in current object
at :: Text -> Builder -> Builder
at k b (A.Object o) = b (o H.! k)
at _ _ _ = error "Expecting object (at)"

-- | Get value in current object
attr :: Text -> Builder
attr k (A.Object o) = o H.! k
attr _ _ = error "Expecting object (attr)"

-- | Filter current object by keys
keep :: [Text] -> Builder
keep ks (A.Object o) = A.Object $ H.filterWithKey (const . (`elem` ks)) o
keep _ _ = error "Expecting object (keep)"

-- | Map over input array
map :: Builder -> Builder
map b (Array a) = Array $ Vec.map b a
map _ _ = error "Expecting array (map)"

-- | Get value at index of current array
index :: Int -> Builder
index i (Array a) = a Vec.! i
index _ _ = error "Expecting array (index)"

-- | Move to index in current array
atIndex :: Int -> Builder -> Builder
atIndex i b (Array a) = b $ a Vec.! i
atIndex _ _ _ = error "Expecting array (atIndex)"

-- | Produce object with given keys
obj :: H.HashMap Text Builder -> Builder
obj h x = A.Object $ H.map (\b -> b x) h

-- | Combine two objects
merge :: Builder -> Builder -> Builder
merge a b val = case (a val, b val) of
                  (Object c, Object d) -> Object $ H.union c d
                  _ -> error "Expected two objects (merge)"


-- $use
--
-- Filter unwanted attributes from an object
--
-- > keep ["nice", "good"]
-- >
-- > -- { bad: 3, good: 1, nice: 500, evil: -3 }
-- > -- => { good: 1, nice: 500 }
--
-- Grab value
--
-- > attr "foo"
-- >
-- > -- { foo: 2 } => 2
--
-- Dig deeper
--
-- > at "foo" $ attr "bar"
-- >
-- > -- { foo: { bar: 3 }} => 3
--
-- Map stuff
--
-- > map $ attr "foo"
-- >
-- > -- [{ foo:1 }, { foo:2 }] => [1, 2]
--
-- Extract indices
--
-- > map $ index 0
-- >
-- > -- [[1,2], [3,4]] => [1, 3]
--
-- Create object
--
-- > obj $ fromList [
-- >     ("first", index 0)
-- >   , ("second", index 1)
-- >   ]
-- >
-- > -- ["hi", "bye"] => { first:"hi", second:"bye" }
--
-- Transform position
--
-- > at "ranks" $ atIndex 0 $ attr "name"
-- >
-- > -- {ranks: [ { name:"winner", score:12 }, { name:"loser", score: 3 ]}
-- > -- => "winner"
--
-- Combine objects
--
-- > merge (attr "foo") (attr "bar")
-- >
-- > -- { foo: { a:1, b:2 }, bar: { b:3, c:4 } }
-- > -- => { a:1, b:2, c:4 }
