{-# LANGUAGE TypeInType, TypeOperators, TemplateHaskell #-}

module Clr.Bindings.ImportGen where

import Clr.Types

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Data.IORef
import Foreign.C.String
import Foreign.Ptr
import System.IO.Unsafe(unsafePerformIO)

importGen = QuasiQuoter { quoteExp  = undefined
                        , quotePat  = undefined
                        , quoteType = undefined
                        , quoteDec  = importGenDec }

clrNameCounter :: IORef Int
{-# NOINLINE clrNameCounter #-}
clrNameCounter = unsafePerformIO (newIORef 0)

newMakeClrFunName' :: IO Name
newMakeClrFunName' = do
  n <- readIORef clrNameCounter
  writeIORef clrNameCounter $ n+1
  return $ mkName $ "makeClrFun_" ++ show n

newMakeClrFunName :: Q Name
newMakeClrFunName = runIO newMakeClrFunName'

importGenDec :: String -> Q [Dec]
importGenDec s = do
                let sig1 = [t|CString -> IO ()|] :: Q Type
                sig2 <- [t|(FunPtr $sig1) -> ($sig1)|]
                --let name1 = mkName "makeClrFun"
                --let name2 = mkName "makeClrFun"
                name1 <- newMakeClrFunName
                name2 <- newMakeClrFunName
                dec1 <- return $ ForeignD $ ImportF CCall Safe "dynamic" name1 sig2
                dec2 <- return $ ForeignD $ ImportF CCall Safe "dynamic" name2 sig2
                --dec1 <- [d| foreign import ccall "dynamic" makeClrFun :: $sig2|]
                --dec2 <- [d| foreign import ccall "dynamic" makeClrFun :: $sig2|]
                return $ [dec1, dec2]



--(ConT (mkName "FunPtr") `AppT` (ParensT (ContT (mkName "CString") `AppT` ArrowT `AppT` (TuppleT 0)) ) ) ]

--[ForeignD (ImportF CCall Safe "dynamic" makeWriteLineType1_2 (AppT (AppT ArrowT (AppT (ConT GHC.Ptr.FunPtr) (AppT (AppT ArrowT (ConT Foreign.C.String.CString)) (AppT (ConT GHC.Types.IO) (TupleT 0))))) (AppT (AppT ArrowT (ConT Foreign.C.String.CString)) (AppT (ConT GHC.Types.IO) (TupleT 0)))))]

