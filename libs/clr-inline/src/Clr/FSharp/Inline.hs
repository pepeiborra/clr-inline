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

fsharp :: QuasiQuoter
fsharp = fsharp' defaultConfig

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
  
