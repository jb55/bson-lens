{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Bson.Lens
( _Float
, _String
, _Doc
, _Array
, _Bin
, _Fun
, _Uuid
, _Md5
, _UserDef
, _ObjId
, _Bool
, _UTC
, _Null
, _RegEx
, _JavaScr
, _Sym
, _Int32
, _Int64

, key
)
where

import Data.Bson
import Control.Lens
import Data.Text
-- import Data.Profunctor

$(makePrisms ''Value)

newtype IndexedDocument = IndexedDocument { runIndexedDocument :: Document }

-- instance Wrapped IndexedDocument where
--   type Unwrapped IndexedDocument = Document
--   _Wrapped' = iso runIndexedDocument IndexedDocument

type instance IxValue IndexedDocument = Value
type instance Index IndexedDocument = Text
instance Ixed IndexedDocument where
  ix k f xs0 = IndexedDocument <$> go (runIndexedDocument xs0) k
    where
      go [] _     = pure []
      go (kv@(k' := v):as) k
        | k == k'   = ((:=) <$> pure k' <*> f v) <&> (:as)
        | otherwise = (kv:) <$> go as k

_IndexedDocument :: Iso' Document IndexedDocument 
_IndexedDocument = iso IndexedDocument runIndexedDocument

key :: Label -> Traversal' Value Value
key field = _Doc . _IndexedDocument . ix field
