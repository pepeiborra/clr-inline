module Clr.Inline
  ( csharp
  , csharp'
  , fsharp
  , fsharp'
  , startClr
  -- * Reexports for generated code
  , getMethodStubRaw
  , FunPtr
  , BStr(..)
  , TextBStr(..)
  , Object(..)
  , ObjectPtr(..)
  )where

import Clr.Host
import Clr.FSharp.Inline
import Clr.CSharp.Inline
import Clr.Inline.Types
