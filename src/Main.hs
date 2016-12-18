{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications #-}

module Main where

import Clr
import Clr.Host

import Foreign.C.String
import Foreign.Ptr

instance MethodS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) where
  type ResultTypeS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeS1 x = stub >>= \f-> f x >> return ()

type WLT = CString -> IO ()
foreign import ccall "dynamic" makeWLT :: FunPtr WLT -> WLT

stub :: IO WLT
stub = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.String" >>= return . makeWLT

main :: IO ()
main = do
  startClr
  invokeS @"WriteLine" @"System.Console" "Hello CLR!!!"
