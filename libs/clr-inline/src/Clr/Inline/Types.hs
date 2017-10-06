{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}
{-# OPTIONS -Wno-partial-type-signatures #-}
module Clr.Inline.Types
  ( ClrPtr(..)
  , Clr(..)
  , ClrType(..)
  , Quotable
  , TextBStr(..)
  , lookupDelegateMarshalType
  , lookupQuotableClrType
  , lookupQuotableMarshalType
  , lookupQuotableUnmarshalType
  ) where

import           Clr.Host.GCHandle
import           Clr.Host.BStr
import           Clr.Marshal
import           Data.Coerce
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Word
import           Foreign
import           GHC.TypeLits
import           Language.Haskell.TH
import           System.IO.Unsafe
import           Text.Printf

-- | A pointer to a Clr object.
--   The only way to access the contents is via clr-inline quotations.
newtype ClrPtr (name::Symbol)= ClrPtr (GCHandle Int)

-- | A wrapper around a 'ClrPtr', which will be released once this
--   wrapper is no longer referenced.
--   The only way to access the contents is in clr-inline quotations.
newtype Clr (name::Symbol) = Clr (ForeignPtr Int)

-- Returning from a CLR function that was called from Haskell
instance Unmarshal (ClrPtr n) (Clr n) where
  unmarshal (ClrPtr id) = do
    ptr <- newForeignPtr (unsafeDupablePerformIO gcHandleFinalizer) (coerce id)
    return (Clr ptr)

-- Returning from a Haskell function that was called by the CLR
instance {-# OVERLAPPING #-} Unmarshal (Clr t) (ClrPtr t) where
  unmarshal (Clr x) = withForeignPtr x $ \x'-> ClrPtr <$> newHandle (coerce x')

-- Calling a CLR function from Haskell
instance Marshal (Clr n) (ClrPtr n) where
  marshal (Clr ptr) f = withForeignPtr ptr $ \p -> f (ClrPtr $ coerce p)

-- Calling a Haskell function from the CLR
instance {-# OVERLAPPING #-} Marshal (ClrPtr t) (Clr t) where
  marshal (ClrPtr x) f = do
    x' <- newHandle x
    fp <- newForeignPtr (unsafeDupablePerformIO gcHandleFinalizer) (coerce x')
    f $ Clr fp

newtype ClrType = ClrType {getClrType :: String} deriving Show
type MarshalType = Type

newtype TextBStr = TextBStr BStr
instance Unmarshal TextBStr Text where unmarshal (TextBStr t) = unmarshal t
instance Marshal Text TextBStr where marshal x f = marshal x (f . TextBStr)

-- | Extensible mapping between quotable CLR types and Haskell types
class Unmarshal marshal unmarshal =>
         Quotable (quoted::Symbol) (clr::Symbol)    marshal     unmarshal
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
instance Quotable "unit" "Microsoft.FSharp.Core.Unit" ()        ()
-- | All reference types are handled by this instance.
instance Quotable a a (ClrPtr a)  (Clr a)

lookupQuotable :: Show a => ([InstanceDec] -> a) -> String -> Q a
lookupQuotable extract quote = do
  let ty = LitT (StrTyLit quote)
  a <- newName "clr"
  b <- newName "rep"
  c <- newName "haskell"
  instances <- reifyInstances ''Quotable [ ty, VarT a, VarT b, VarT c ]
  return $ extract instances

handleOverlappingInstances :: String -> String -> [Dec] -> a
handleOverlappingInstances msg s instances = error $ printf "Overlapping %s instances for Quotable %s: %s" msg s (show names) -- (show instances)
  where
    names = [ quote | InstanceD _ _ (_ `AppT` quote `AppT` _ `AppT` _ `AppT` _) _ <- instances ]

extractMostSpecificInstance :: String -> String -> _ -> _ -> [Dec] -> _
extractMostSpecificInstance s msg f1 f2 instances =
  fromMaybe (handleOverlappingInstances msg s instances) . getFirst $
  foldMap (apply f1) instances <> foldMap (apply f2) instances
  where
    apply f (InstanceD _ _ (_ `AppT` quote `AppT` clr `AppT` marshal `AppT` unmarshal) _) = First $ f quote clr marshal unmarshal
    apply _ _ = error "unreachable"

lookupQuotableClrType :: String -> Q ClrType
lookupQuotableClrType s = lookupQuotable extract s
    where
      extract = extractMostSpecificInstance s "Clr" specific general
      specific _ (LitT (StrTyLit s)) _ _ = Just $ ClrType s
      specific _ _ _ _ = Nothing
      general quote@VarT{} clr@VarT{} _ _ | quote == clr = Just $ ClrType s
      general _ _ _ _ = Nothing

lookupQuotableMarshalType :: String -> Q MarshalType
lookupQuotableMarshalType s = lookupQuotable extract s
  where
    extract = extractMostSpecificInstance s "Marshal" specific general
    specific LitT{} LitT{} marshalTy _ = Just marshalTy
    specific _ _ _ _ = Nothing
    general quote@VarT{} clr@VarT{} (con `AppT` v)  _ | quote == clr && quote == v= Just $ AppT con (LitT (StrTyLit s))
    general _ _ _ _ = Nothing

lookupDelegateMarshalType :: [String] -> TypeQ -> Q MarshalType
lookupDelegateMarshalType args resTy =
      foldr (\t u -> arrowT `appT` lookupQuotableMarshalType t `appT` u) [t| IO $(resTy) |] args

lookupQuotableUnmarshalType :: String -> Q Type
lookupQuotableUnmarshalType s = lookupQuotable extract s
  where
    extract = extractMostSpecificInstance s "Unmarshal" specific general
    specific LitT{} LitT{} _ unmarshalTy = Just unmarshalTy
    specific _ _ _ _ = Nothing
    general quote@VarT{} clr@VarT{} _ (con `AppT` v) | quote == clr && quote == v = Just $ AppT con (LitT (StrTyLit s))
    general _ _ _ _ = Nothing
