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
    Id
  | At Text Builder
  | Attr Text
  | Attrs [Text]
  | Map Builder
  | Index Int Builder
  | Obj (H.HashMap Text Builder)

-- | Generates new Aeson 'Value' guided by a 'Builder'
--
transform :: Builder -> Value -> Value
transform (At k b)    (A.Object o) = transform b $ o H.! k
transform (At _ _)    _            = error "Expecting object (At)"
transform (Index i b) (Array a)    = transform b $ a Vec.! i
transform (Index _ _) _            = error "Expecting array (Index)"
transform (Map b)     (Array a)    = Array $ Vec.map (transform b) a
transform (Map _)     _            = error "Expecting array (Map)"
transform (Attr k)    (A.Object o) = o H.! k
transform (Attr _)    _            = error "Expecting object (Attr)"
transform (Attrs ks)  (A.Object o) = A.Object $ H.filterWithKey (const . (`elem` ks)) o
transform (Attrs _)   _            = error "Expecting object (Attrs)"
transform (Obj fs)    x            = A.Object $ H.map (`transform` x) fs
transform Id          x            = x

-- $use
--
-- Filter unwanted attributes from an object
--
-- > Attrs ["nice", "good"]
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
-- > Map $ Index 0 Id
-- >
-- > -- [[1,2], [3,4]] => [1, 3]
--
-- Create object
--
-- > Obj $ fromList [
-- >     ("first", Index 0 Id)
-- >   , ("second", Index 1 Id)
-- >   ]
-- >
-- > -- ["hi", "bye"] => { first:"hi", second:"bye" }
