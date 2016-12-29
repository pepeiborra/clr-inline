{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications #-}

module Main where

import Clr
import Clr.Host

import Data.Int(Int32)
import Foreign.C.String(CString)
import Foreign.Ptr(FunPtr)

instance MethodS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeS1 x = stub1 >>= \f-> f x >> return ()

foreign import ccall "dynamic" makeWriteLineType1 :: FunPtr (CString -> IO ()) -> (CString -> IO ())

stub1 :: IO (CString -> IO ())
stub1 = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.String" >>= return . makeWriteLineType1

instance MethodS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) where
  type ResultTypeS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) = 'Nothing
  rawInvokeS1 x = stub2 >>= \f-> f x >> return ()

foreign import ccall "dynamic" makeWriteLineType2 :: FunPtr (Int32 -> IO ()) -> (Int32 -> IO ())

stub2 :: IO (Int32 -> IO ())
stub2 = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.Int32" >>= return . makeWriteLineType2

main :: IO ()
main = do
  startClr
  invokeS @"WriteLine" @"System.Console" "Hello CLR!!!"
  invokeS @"WriteLine" @"System.Console" (2 :: Int32)   -- overloaded
