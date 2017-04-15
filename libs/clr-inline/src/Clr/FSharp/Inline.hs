{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
--  declares the return type. An F# expression quotation can refer to
--  a Haskell binding @x@ using the syntax @($x:type)@ where type is
--  an F# type and is only required on the first usage, and the parentheses
--  are optional. An antiquotation @$x:type@
--  is well-scoped if there exists a variable @x@ with a compatible type in
--  the Haskell context. An F# expression always returns in the IO monad.
--  Example expressions:
--
-- @
-- hello :: IO (Int, Clr "System.DateTime")
-- hello = do
--   let year = 2017 :: Int
--   anClr <- [fsharp| DateTime{ DateTime($year:int,04,10)} |]
--   anInt <- [fsharp| int{ ($anClr:System.DateTime).Year + $year:int + $year}|]
--   return (anInt, anClr)
-- @
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
  
