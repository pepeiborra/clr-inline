{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bindings where

import Clr

import Foreign.C
import Data.Int

writeLineRaw1 :: CString -> IO ()
writeLineRaw1 cs = putStrLn "static method called with 1 string"
writeLineRaw2 :: CString -> CString -> IO ()
writeLineRaw2 cs1 cs2 = putStrLn "static method called with 2 strings"

instance MethodS "System.Console" "WriteLine" '["System.String"] where
  type ResultTypeS "System.Console" "WriteLine" '["System.String"] = 'Nothing
  rawInvokeS = writeLineRaw1

instance MethodS "System.Console" "WriteLine" '["System.String", "System.String"] where
  type ResultTypeS "System.Console" "WriteLine" '["System.String", "System.String"] = 'Nothing
  rawInvokeS = writeLineRaw2

type instance SuperTypeOf "MyInstance" = 'Just "System.Object"

instance MethodI "MyInstance" "MakeSound" '["System.String"] where
  type ResultTypeI "MyInstance" "MakeSound" '["System.String"] = 'Nothing
  rawInvokeI = rawInvokeMyInstanceStr

instance MethodI "MyInstance" "MakeSound" '["System.Int64"] where
  type ResultTypeI "MyInstance" "MakeSound" '["System.Int64"] = 'Nothing
  rawInvokeI = rawInvokeMyInstanceInt64

instance MethodI "MyInstance" "MakeSound" '["System.Int32"] where
  type ResultTypeI "MyInstance" "MakeSound" '["System.Int32"] = 'Nothing
  rawInvokeI = rawInvokeMyInstanceInt32

rawInvokeMyInstanceStr :: ObjectID -> CString -> IO ()
rawInvokeMyInstanceStr d s = putStrLn "instance method called with string"

rawInvokeMyInstanceInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeMyInstanceInt64 d s = putStrLn "instance method called with int64"

rawInvokeMyInstanceInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeMyInstanceInt32 d s = putStrLn "instance method called with int32"
