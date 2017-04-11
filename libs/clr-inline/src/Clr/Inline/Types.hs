{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}
module Clr.Inline.Types where

import           Clr.Bindings.Marshal ()
import           Clr.Host.BStr
import           Clr.Marshal
import           Data.Char
import           Data.Int
import           Data.List.Extra
import           Data.Text            (Text)
import           Data.Word
import           Language.Haskell.TH

newtype Object = Object Int64

type instance UnmarshalAs Object = Object

newtype ClrType = ClrType {getClrType :: String}

toClrType :: Type -> Maybe ClrType
toClrType (ConT t) =
  ClrType <$>
  case () of
    _ | t == ''Bool -> Just "System.Boolean"
    _ | t == ''Double -> Just "System.Double"
    _ | t == ''Int -> Just "System.Int32"
    _ | t == ''Int32 -> Just "System.Int32"
    _ | t == ''Int64 -> Just "System.Int64"
    _ | t == ''TextBStr -> Just "System.String"
    _ | t == ''BStr -> Just "System.String"
    _ | t == ''String -> Just "System.String"
    _ | t == ''Text -> Just "System.String"
    _ | t == ''Object -> Just "System.Object"
    _ | otherwise -> Nothing
toClrType _ = Nothing

newtype TextBStr = TextBStr BStr
type instance UnmarshalAs TextBStr = Text
instance Unmarshal TextBStr Text where unmarshal (TextBStr t) = unmarshal t

-- | Rudimentary parser for stringy Haskell types.
--   If successful, produces two types:
--     1. when parsing a return type
--     2. when parsing an argument type
toTHType :: String -> Maybe (TypeQ,TypeQ)
toTHType (map toLower . trim -> s) =
  case s of
    "string" -> Just ([t|BStr|]     ,[t|BStr|])
    "text"   -> Just ([t|TextBStr|] ,[t|BStr|])
    "double" -> Just ([t|Double|]   ,[t|Double|])
    "bool"   -> Just ([t|Bool|]     ,[t|Bool|])
    "int"    -> Just ([t|Int|]      ,[t|Int|])
    "int32"  -> Just ([t|Int32|]    ,[t|Int32|])
    "int64"  -> Just ([t|Int64|]    ,[t|Int64|])
    "word"   -> Just ([t|Word64|]   ,[t|Word64|])
    "object" -> Just ([t|Object|]   ,[t|Object|])
    _        -> Nothing

class InlineMarshal a b where
  inlineMarshal :: a -> (b -> IO c) -> IO c
