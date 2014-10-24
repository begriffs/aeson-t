module Data.Aeson.Transform
  (
    -- * Example usage
    -- $use

    -- * Transformation builder
    Builder(..)
    -- * Executing transformation
  , transform
  ) where

import Data.Aeson as A
import Data.Text
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as H

-- | Transformations are specified by creating 'Builder' instances.
-- Builders specify how to navigate through input JSON and construct
-- output at various nodes in the tree.
--
data Builder =
    Id                  -- ^ Pass input directly as output
  | At Text Builder     -- ^ Move to value in current object
  | Attr Text           -- ^ Get value in current object
  | Keep [Text]         -- ^ Filter current object by keys
  | Map Builder         -- ^ Map over input array
  | Index Int           -- ^ Get value at index of current array
  | AtIndex Int Builder -- ^ Move to index in current array
  | Obj (H.HashMap Text Builder) -- ^ Produce object with given keys
  | Merge Builder Builder  -- ^ Combine two objects


-- | Generates new Aeson 'Value' guided by a 'Builder'
--
transform :: Builder -> Value       -> Value
transform Id            x            = x
transform (At k b)      (A.Object o) = transform b $ o H.! k
transform (At _ _)      _            = error "Expecting object (At)"
transform (AtIndex i b) (Array a)    = transform b $ a Vec.! i
transform (AtIndex _ _) _            = error "Expecting array (AtIndex)"
transform (Index i)     (Array a)    = a Vec.! i
transform (Index _)     _            = error "Expecting array (Index)"
transform (Map b)       (Array a)    = Array $ Vec.map (transform b) a
transform (Map _)       _            = error "Expecting array (Map)"
transform (Attr k)      (A.Object o) = o H.! k
transform (Attr _)      _            = error "Expecting object (Keep)"
transform (Keep ks)     (A.Object o) = A.Object $ H.filterWithKey (const . (`elem` ks)) o
transform (Keep _)      _            = error "Expecting object (Keep)"
transform (Obj fs)      x            = A.Object $ H.map (`transform` x) fs
transform (Merge a b)   x            = case (transform a x, transform b x) of
                                       (Object c, Object d) -> Object $ H.union c d
                                       _ -> error "Expected two objects (Merge)"

-- $use
--
-- Filter unwanted attributes from an object
--
-- > Keep ["nice", "good"]
-- >
-- > -- { bad: 3, good: 1, nice: 500, evil: -3 }
-- > -- => { good: 1, nice: 500 }
--
-- Grab value
--
-- > Attr "foo"
-- >
-- > -- { foo: 2 } => 2
--
-- Dig deeper
--
-- > At "foo" $ Attr "bar"
-- >
-- > -- { foo: { bar: 3 }} => 3
--
-- Map stuff
--
-- > Map $ Attr "foo"
-- >
-- > -- [{ foo:1, foo:2 }] => [1, 2]
--
-- Extract indices
--
-- > Map $ Index 0
-- >
-- > -- [[1,2], [3,4]] => [1, 3]
--
-- Create object
--
-- > Obj $ fromList [
-- >     ("first", Index 0)
-- >   , ("second", Index 1)
-- >   ]
-- >
-- > -- ["hi", "bye"] => { first:"hi", second:"bye" }
--
-- Transform position
--
-- > At "ranks" $ AtIndex 0 $ Attr "name"
-- >
-- > -- {ranks: [ { name:"winner", score:12 }, { name:"Loser", score: 3 ]}
-- > -- => "winner"
--
-- Combine objects
--
-- > Merge (Attr "foo") (Attr "bar")
-- >
-- > -- { foo: { a:1, b:2 }, bar: { b:3, c:4 } }
-- > -- => { a:1, b:3, c:4 }
