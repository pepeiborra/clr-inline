{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Clr.FSharp.Inline (fsharp, fsharp', getMethodStub, FunPtr) where

import Clr.Bindings
import Clr.Inline.Config
import Clr.Inline.Types
import Clr.FSharp.Gen
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Foreign

fsharp = fsharp' defaultInlineConfig

fsharp' cfg = QuasiQuoter
    { quoteExp  = fsharpExp cfg
    , quotePat  = error "Clr.FSharp.Inline: quotePat"
    , quoteType = error "Clr.FSharp.Inline: quoteType"
    , quoteDec  = fsharpDec cfg
    }

fsharpExp :: ClrInlineConfig -> String -> Q Exp
fsharpExp = clrQuoteExp "fsharp" . compile
fsharpDec = clrQuoteDec "fsharp" . compile
