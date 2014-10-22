module Data.Aeson.Transform
  ( Builder(..)
  , transform
  ) where

import Data.Aeson as A
import Data.Text
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as H

data Builder =
    Id
  | At Text Builder
  | Attr Text
  | Attrs [Text]
  | Map Builder
  | Index Int Builder
  | Obj (H.HashMap Text Builder)

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
