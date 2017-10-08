
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
module Clr.Inline.Types.Quote
  ( Quotable
  , lookupDelegateMarshalType
  , lookupQuotableClrType
  , MarshalType
  , lookupQuotableMarshalType
  , lookupQuotableUnmarshalType
  ) where

import           Clr.Host.BStr
import           Clr.Marshal
import           Clr.Inline.Types
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Word
import           GHC.TypeLits
import           Language.Haskell.TH
import           Text.Printf

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
instance Quotable a                a                (ClrPtr a)  (Clr a)

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

lookupQuotableClrType :: String -> Q ClrTypeSymbol
lookupQuotableClrType s = lookupQuotable extract s
    where
      extract = extractMostSpecificInstance s "Clr" specific general
      specific _ (LitT (StrTyLit s)) _ _ = Just $ ClrTypeSymbol s
      specific _ _ _ _ = Nothing
      general quote@VarT{} clr@VarT{} _ _ | quote == clr = Just $ ClrTypeSymbol s
      general _ _ _ _ = Nothing

type MarshalType = Type

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
