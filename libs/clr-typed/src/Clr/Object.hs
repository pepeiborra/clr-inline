{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures, GADTs, TypeInType #-}

module Clr.Object where

import Clr.Marshal
import Data.Kind
import Data.Int

--
-- A unique indentifier for a particular ref type.
-- Just a place holder for now might need to something else such as pointer later on
--
newtype ObjectID typ = ObjectID Int64

--
-- An object is just its unique identifer + information of its type
--
data Object (typ::Type) where
  Object :: ObjectID typ -> Object typ


--
-- Marshaling objects
--
instance {-# OVERLAPS #-} Marshal (Object t) (ObjectID t) where
  marshal (Object x) f = f x

instance {-# OVERLAPPING #-} Unmarshal (ObjectID t) (Object t) where
  unmarshal oid = return $ Object oid
