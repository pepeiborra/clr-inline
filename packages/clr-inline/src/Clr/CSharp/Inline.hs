module Clr.CSharp.Inline (csharp) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

csharp = QuasiQuoter
    { quoteExp = \txt -> error "not yet implemented"
    , quotePat  = error "Clr.CSharp.Inline: quotePat"
    , quoteType = error "Clr.CSharp.Inline: quoteType"
    , quoteDec  = error "Clr.CSharp.Inline: quoteDec"
    }
