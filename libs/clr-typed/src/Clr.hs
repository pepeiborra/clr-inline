{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr
  ( invokeS
  , invokeI
  , new
  , getPropI
  , setPropI
  , getPropS
  , setPropS
  , delegate
  , module Clr.Constructor
  , module Clr.Delegate
  , module Clr.Inheritance
  , module Clr.ListTuple
  , module Clr.Method.Instance
  , module Clr.Method.Static
  , module Clr.Object
  , module Clr.Property
  , module Clr.Types
  ) where

import Clr.Bridge
import Clr.Constructor
import Clr.Curry
import Clr.Delegate
import Clr.Inheritance
import Clr.ListTuple
import Clr.Marshal
import Clr.MarshalF
import Clr.Method.Instance
import Clr.Method.Static
import Clr.Object
import Clr.Property
import Clr.Resolver
import Clr.Types

