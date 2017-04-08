{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Clr.FSharp.Inline
  ( fsharp
  , fsharp'
  , getMethodStub
  , FunPtr
  , BStr(..)
  , unmarshalAuto
  ) where

import           Clr.Bindings
import           Clr.FSharp.Gen
import           Clr.Inline.Config
import           Clr.Inline.Quoter
import           Foreign
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

fsharp :: QuasiQuoter
fsharp = fsharp' defaultInlineConfig

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
    (configForceReturnType cfg)
    (compile cfg)

fsharpDec :: ClrInlineConfig -> String -> Q [Dec]
fsharpDec = clrQuoteDec name . compile
