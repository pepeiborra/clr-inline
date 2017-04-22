{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clr.FSharp.Inline
  ( fsharp
  , fsharp'
  ) where

import           Clr.FSharp.Gen
import           Clr.Inline.Config
import           Clr.Inline.Quoter
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

-- | F# declaration and expression quasiquoter.
--   Declarations can include open statements, types or even modules.
--  Example declaration:
--
-- @
-- [fsharp|
--   open System
--   open System.Collections.Generic
--   module Globals =
--      let mutable today = DateTime.Today
-- |]
-- @
--
--  Expressions are wrapped in a curly braces block @{}@ that
--  fixes the return type. An F# expression quotation can refer to
--  a Haskell binding @x@ using the syntax @($x:type)@ where type is
--  a string denoting an F# type and is only required on the first usage, and the parentheses
--  are optional. F# types are mapped to Haskell types via the 'Quotable' class.
--  An antiquotation @$x:type@
--  is well-scoped if there exists a variable @x@ with a Haskell type @U@ in
--  the Haskell context such that there exists an instance
--  @Quotable type clr marshall U@ for some @clr@ and @marshall@.
--
--  An F# expression returns an IO computation that
--  produces a value of the quoted result type if said type is 'Quotable'.
--  Example expressions:
--
-- @
-- hello :: IO (Int, Clr \"System.DateTime")
-- hello = do
--   let year = 2017 :: Int
--   aClr <- [fsharp| DateTime{ DateTime($year:int,04,10)} |]
--   anInt <- [fsharp| int{ ($aClr:System.DateTime).Year + $year:int + $year}|]
--   return (anInt, anClr)
-- @
--
--  CLR Reference types are modelled in Haskell as 'Clr' values, indexed with the
--  name of their F# type as a type level symbol. String equivalence is a poor
--  substitute for type equality, so for two 'Clr' values to have the same type
--  they must be indexed by exactly the same string.
--
--  This quasiquoter is implicitly configured with the 'defaultConfig'.
fsharp :: QuasiQuoter
fsharp = fsharp' defaultConfig

-- | Explicit configuration version of 'fsharp'.
fsharp' :: ClrInlineConfig -> QuasiQuoter
fsharp' cfg = QuasiQuoter
    { quoteExp  = fsharpExp cfg
    , quotePat  = error "Clr.FSharp.Inline: quotePat"
    , quoteType = error "Clr.FSharp.Inline: quoteType"
    , quoteDec  = fsharpDec cfg
    }

fsharpExp :: ClrInlineConfig -> String -> Q Exp
fsharpExp cfg =
  clrQuoteExp
    name
    (compile cfg)

fsharpDec :: ClrInlineConfig -> String -> Q [Dec]
fsharpDec = clrQuoteDec name . compile
  
