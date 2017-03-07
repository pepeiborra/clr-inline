{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Clr.FSharp.Inline (fsharp, getMethodStub, FunPtr) where

import Control.Monad.IO.Class
import Clr.Bindings
import Clr.Inline.State
import Clr.Inline.Utils
import Clr.Inline.Types
import Clr.FSharp.Gen
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Foreign
import Text.Printf


fsharp = QuasiQuoter
    { quoteExp  = fsharpExp
    , quotePat  = error "Clr.FSharp.Inline: quotePat"
    , quoteType = error "Clr.FSharp.Inline: quoteType"
    , quoteDec  = error "Clr.FSharp.Inline: quoteDec"
    }

-- | Quasiquoter for expressions. Responsible for:
--      - Installing a finalizer to generate the bytecodes
--      - Generating the foreign import wrapper.
--      - Splicing in the computation that loads the bytecodes, gets a function pointer through the keyhole, and calls it.
fsharpExp :: String -> Q Exp
fsharpExp = clrQuoteExp compile
