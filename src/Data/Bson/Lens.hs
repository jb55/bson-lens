{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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
, values
, docValues
, fields

, AsValue(..)
, AsDocument(..)
)
where

import Data.Bson
import Control.Lens
import Data.Text
-- import Data.Profunctor

$(makePrisms ''Value)


class AsDocument t where
  _Document :: Traversal' t Document

instance AsDocument Value where
  _Document = _Doc

instance AsDocument Document where
  _Document = id

instance AsDocument Javascript where
  _Document = _JavascriptDoc

class AsValue t where
  _Value :: Traversal' t Value

instance AsValue Value where
  _Value = id

instance AsValue Field where
  _Value = lens (\(_ := v)   -> v)
                (\(k := _) v -> k := v)

_Label :: Lens' Field Label
_Label = lens (\(k := _)   -> k)
              (\(_ := v) k -> k := v)

_JavascriptDoc :: Lens' Javascript Document
_JavascriptDoc = lens (\(Javascript d _) -> d)
                      (\(Javascript _ v) d -> Javascript d v)

_Val :: (AsValue t, Val v) => Traversal' t v
_Val = _Value.prism' val cast'

_IndexedDocument :: Iso' Document IndexedDocument
_IndexedDocument = iso IndexedDocument runIndexedDocument

_IndexedDoc :: AsValue v => Traversal' v IndexedDocument
_IndexedDoc = _Value . _Doc . _IndexedDocument

_Symbol :: AsValue v => Traversal' v Text
_Symbol = _Value . _Sym . iso (\(Symbol t) -> t) Symbol

field :: AsDocument t => Label -> Traversal' t Field
field k = _Document . _IndexedDocument . ix k

key :: AsDocument t => Label -> Traversal' t Value
key k = field k . _Value

docValues :: AsDocument v => Traversal' v Value
docValues = fields . _Value

fields :: AsDocument v => Traversal' v Field
fields = _Document . traversed

values :: AsValue v => IndexedTraversal' Int v Value
values = _Value . _Array . traversed

type instance IxValue Value = Value
type instance Index Value = Text
instance Ixed Value where
  ix i = _IndexedDoc . ix i . _Value

newtype IndexedDocument = IndexedDocument { runIndexedDocument :: Document }
type instance IxValue IndexedDocument = Field
type instance Index IndexedDocument = Text
instance Ixed IndexedDocument where
  ix i f xs0 = IndexedDocument <$> go (runIndexedDocument xs0) i
    where
      go [] _     = pure []
      go (kv@(k' := v):as) k
        | i == k'   = f (k := v) <&> (:as)
        | otherwise = (kv:) <$> go as i
