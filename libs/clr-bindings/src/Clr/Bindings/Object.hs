{-# LANGUAGE TypeInType, TypeApplications, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TypeOperators, ScopedTypeVariables, AllowAmbiguousTypes #-}

module Clr.Bindings.Object
 ( objectToString
 , objectGetType
 , downCast
 ) where

import Clr
import Clr.TypeString

import Clr.Marshal

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.DynImports
import Clr.Bindings.BStr
import Clr.Bindings.Object.Internal

import Data.Text as T

--
-- System.Object.ToString()
--
objectToString :: (t  `Implements` T_object ~ 'True) => Object t -> IO T.Text
objectToString o = do
  let o' = upCast o :: Object T_object
  invokeI @T_ToString o' ()

--
-- System.Object.GetType()
--
objectGetType :: (t  `Implements` T_object ~ 'True) => Object t -> IO (Object T_Type)
objectGetType o = do
  let o' = upCast o :: Object T_object
  invokeI @T_GetType o' ()

--
-- `downCast @typ o` is `Just o` casted to typ if possible, otherwise Nothing
--
downCast :: forall ts t t' .
 ( MakeT ts ~ t'                           -- Shorthand to long notation conversion
 , TString t'                              -- And the result of the above can be turned into a runtime string
 , t  `Implements` T_object ~ 'True        -- Supplied object must at least inherit from Object so we can call Object.GetType
 , t' `Implements` t ~ 'True               -- Ensure the desired cast is not something meaningless like cat -> dog at compile time
 ) => Object t -> IO (Maybe (Object t'))
downCast o = do
  typ  <- objectGetType o
  typ' <- typeGetType (T.pack (tString @t'))
  canDownCast <- typ' `typeIsAssignableFrom` typ
  if canDownCast then
    return $ Just $ unsafeDownCast o
  else
    return Nothing

