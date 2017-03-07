{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Clr.FSharp.Inline (fsharp, getMethodStub, FunPtr) where

import Clr.Bindings
import Clr.Inline.Types
import Clr.FSharp.Gen
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Foreign


fsharp = QuasiQuoter
    { quoteExp  = fsharpExp
    , quotePat  = error "Clr.FSharp.Inline: quotePat"
    , quoteType = error "Clr.FSharp.Inline: quoteType"
    , quoteDec  = error "Clr.FSharp.Inline: quoteDec"
    }

fsharpExp :: String -> Q Exp
fsharpExp = clrQuoteExp "fsharp" compile
