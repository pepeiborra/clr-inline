{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}
module Clr.Inline.Types where

import           Clr.Host.GCHandle
import           Clr.Host.BStr
import           Clr.Marshal
import           Data.Coerce
import           Data.Int
import           Data.Maybe
import           Data.Proxy
import           Data.Text            (Text)
import           Data.Word
import           Foreign
import           GHC.TypeLits
import           Language.Haskell.TH
import           System.IO.Unsafe

-- | A pointer to a Clr object.
--   The only way to access the contents is via clr-inline quotations.
newtype ClrPtr (name::Symbol)= ClrPtr (GCHandle Int)

-- | A wrapper around a 'ClrPtr', which will be released once this
--   wrapper is no longer referenced.
--   The only way to access the contents is in clr-inline quotations.
newtype Clr (name::Symbol) = Clr (ForeignPtr Int)

foreign import ccall "dynamic" releaseObject :: FunPtr (Int64 -> IO ()) -> (Int64 -> IO ())

instance Unmarshal (ClrPtr n) (Clr n) where
  unmarshal (ClrPtr id) = do
    ptr <- newForeignPtr (unsafeDupablePerformIO gcHandleFinalizer) (coerce id)
    return (Clr ptr)

instance Marshal (Clr n) (ClrPtr n) where
  marshal (Clr ptr) f = withForeignPtr ptr $ \p -> f (ClrPtr $ coerce p)

newtype ClrType = ClrType {getClrType :: String} deriving Show

newtype TextBStr = TextBStr BStr
instance Unmarshal TextBStr Text where unmarshal (TextBStr t) = unmarshal t
instance Marshal Text TextBStr where marshal x f = marshal x (f . TextBStr)

-- | Extensible mapping between quotable CLR types and Haskell types
class Unmarshal marshal haskell =>
         Quotable (quoted::Symbol) (clr::Symbol)    marshal     haskell | marshal -> haskell clr
instance Quotable "bool"           "System.Boolean" Bool        Bool
instance Quotable "double"         "System.Double"  Double      Double
instance Quotable "int"            "System.Int32"   Int         Int
instance Quotable "int16"          "System.Int16"   Int16       Int16
instance Quotable "int32"          "System.Int32"   Int32       Int32
instance Quotable "int64"          "System.Int64"   Int64       Int64
instance Quotable "long"           "System.Int64"   Int64       Int64
instance Quotable "uint16"         "System.UInt16"  Word16      Word16
instance Quotable "word16"         "System.UInt16"  Word16      Word16
instance Quotable "uint32"         "System.UInt32"  Word32      Word32
instance Quotable "word32"         "System.UInt32"  Word32      Word32
instance Quotable "uint64"         "System.UInt64"  Word64      Word64
instance Quotable "word64"         "System.UInt64"  Word64      Word64
instance Quotable "string"         "System.String"  BStr        String
instance Quotable "text"           "System.String"  TextBStr    Text
instance Quotable "void"           "System.Void"    ()          ()
-- | All reference types are handled by this instance.
instance Quotable a                a                (ClrPtr a)  (Clr a)

lookupQuotable :: Show a => ([InstanceDec] -> a) -> String -> Q a
lookupQuotable extract quote = do
  let ty = LitT (StrTyLit quote)
  a <- newName "clr"
  b <- newName "rep"
  c <- newName "haskell"
  instances <- reifyInstances ''Quotable [ ty, VarT a, VarT b, VarT c ]
  return $ extract instances

lookupQuotableClrType :: String -> Q ClrType
lookupQuotableClrType s = lookupQuotable extractClrType s
    where
      extractClrType instances = fromMaybe (general instances) $ listToMaybe $ mapMaybe specific instances
      specific (InstanceD _ _ (_ `AppT` _ `AppT` LitT (StrTyLit s) `AppT` _ `AppT` _) _) = Just $ ClrType s
      specific _ = Nothing
      general [InstanceD _ _ (_ `AppT` quote `AppT` clr `AppT` _ `AppT` _) _] | quote == clr = ClrType s
      general _ = error $ "Overlapping instances for Quotable " ++ s

lookupQuotableMarshalType :: String -> Q Type
lookupQuotableMarshalType s = lookupQuotable extractMarshalType s
  where
    extractMarshalType instances = fromMaybe (general instances) $ listToMaybe $ mapMaybe specific instances
    specific (InstanceD _ _ (_ `AppT` quote `AppT` clr `AppT` _ `AppT` _) _) | quote == clr = Nothing
    specific (InstanceD _ _ (_ `AppT` _ `AppT` _ `AppT` marshalTy `AppT` _) _) = Just marshalTy
    specific _ = Nothing
    general [InstanceD _ _ (_ `AppT` quote `AppT` clr `AppT` AppT (ConT clrPtr) _ `AppT` _) _] | quote == clr && clrPtr == ''ClrPtr = AppT (ConT clrPtr) (LitT (StrTyLit s))
    general _ = error $ "Overlapping instances for Quotable " ++ s

unmarshalAuto :: Quotable quote clr a unmarshal => Proxy quote -> a -> IO unmarshal
unmarshalAuto _ = unmarshal
