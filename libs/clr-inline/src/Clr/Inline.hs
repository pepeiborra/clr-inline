module Clr.Inline
  ( csharp
  , csharp'
  , fsharp
  , fsharp'
  , startClr
  -- * Reexports for generated code
  , FunPtr
  , BStr(..)
  , TextBStr(..)
  , Clr(..)
  , ClrPtr(..)
  )where

import           Clr.CSharp.Inline
import           Clr.FSharp.Inline
import           Clr.Host
import           Clr.Host.BStr
import           Clr.Inline.Types
import           Foreign
