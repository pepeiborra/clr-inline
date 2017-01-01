{-# LANGUAGE TypeApplications, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Clr.Bindings.Box where

import Clr.Host.Box

import Clr.Marshal
import Clr.Object
import Clr.Types

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Ptr


--
-- CString
--

instance Marshal CString (ObjectID obj) where
  marshal x f = boxStringStub >>= \stub-> stub x >>= f

type BoxStringStub a = CString -> IO (ObjectID a)
foreign import ccall "dynamic" makeBoxStringStub :: FunPtr (BoxStringStub a) -> (BoxStringStub a)

boxStringStub :: IO (BoxStringStub a)
boxStringStub = getBoxStub "System.String" >>= return . makeBoxStringStub

--
-- String
--

instance Marshal String (ObjectID a) where
  marshal s f = marshal @String @CString s $ \cs-> marshal @CString cs $ \obj-> f obj

--
-- Int32
--

instance Marshal Int32 (ObjectID obj) where
  marshal x f = boxInt32Stub >>= \stub-> stub x >>= f

type BoxInt32Stub a = Int32 -> IO (ObjectID a)
foreign import ccall "dynamic" makeBoxInt32Stub :: FunPtr (BoxInt32Stub a) -> (BoxInt32Stub a)

boxInt32Stub :: IO (BoxInt32Stub a)
boxInt32Stub = getBoxStub "System.Int32" >>= return . makeBoxInt32Stub

--
-- Int64
--

instance Marshal Int64 (ObjectID obj) where
  marshal x f = boxInt64Stub >>= \stub-> stub x >>= f

type BoxInt64Stub a = Int64 -> IO (ObjectID a)
foreign import ccall "dynamic" makeBoxInt64Stub :: FunPtr (BoxInt64Stub a) -> (BoxInt64Stub a)

boxInt64Stub :: IO (BoxInt64Stub a)
boxInt64Stub = getBoxStub "System.Int64" >>= return . makeBoxInt64Stub

--
-- Word32
--

instance Marshal Word32 (ObjectID obj) where
  marshal x f = boxWord32Stub >>= \stub-> stub x >>= f

type BoxWord32Stub a = Word32 -> IO (ObjectID a)
foreign import ccall "dynamic" makeBoxWord32Stub :: FunPtr (BoxWord32Stub a) -> (BoxWord32Stub a)

boxWord32Stub :: IO (BoxWord32Stub a)
boxWord32Stub = getBoxStub "System.UInt32" >>= return . makeBoxWord32Stub

--
-- Word64
--

instance Marshal Word64 (ObjectID obj) where
  marshal x f = boxWord64Stub >>= \stub-> stub x >>= f

type BoxWord64Stub a = Word64 -> IO (ObjectID a)
foreign import ccall "dynamic" makeBoxWord64Stub :: FunPtr (BoxWord64Stub a) -> (BoxWord64Stub a)

boxWord64Stub :: IO (BoxWord64Stub a)
boxWord64Stub = getBoxStub "System.UInt64" >>= return . makeBoxWord64Stub

