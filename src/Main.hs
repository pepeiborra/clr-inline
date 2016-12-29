{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications #-}

module Main where

import Clr
import Clr.Host
import Clr.Resolver
import Clr.Marshal

import Data.Int(Int32, Int64)
import Foreign.C.String(CString)
import Foreign.Ptr(FunPtr)

instance MethodS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeS1 x = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.String" >>= return . makeWriteLineType1 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType1 :: FunPtr (CString -> IO ()) -> (CString -> IO ())

instance MethodS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) where
  type ResultTypeS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) = 'Nothing
  rawInvokeS1 x = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.Int32" >>= return . makeWriteLineType2 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType2 :: FunPtr (Int32 -> IO ()) -> (Int32 -> IO ())

instance MethodS2 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.Object" 'Nothing '[]) where
  type ResultTypeS2 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.Object" 'Nothing '[]) = 'Nothing
  rawInvokeS2 x y = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.String;System.Object" >>= return . makeWriteLineType3 >>= \f-> f x y

foreign import ccall "dynamic" makeWriteLineType3 :: FunPtr (CString -> ObjectID -> IO ()) -> (CString -> ObjectID -> IO ())

type instance Candidates (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) = '[ '[ T "System.String" 'Nothing '[]], '[ T "System.Int32" 'Nothing '[]], '[T "System.String" 'Nothing '[], T "System.Object" 'Nothing '[]]]

main :: IO ()
main = do
  startClr
  invokeS @"WriteLine" @"System.Console" "Hello CLR!!!"
  invokeS @"WriteLine" @"System.Console" (2 :: Int32)        -- overloaded
  --invokeS @"WriteLine" @"System.Console" ("Well", "Hello")   -- overloaded
