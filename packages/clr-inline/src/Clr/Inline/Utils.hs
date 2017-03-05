{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Clr.Inline.Utils where

import           Clr
import           Clr.Bindings
import           Clr.Marshal
import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Char
import           Data.Maybe
import           Foreign
import           GHC.StaticPtr
import           Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax as TH
import           System.IO.Unsafe
import           Text.Printf

-- | A wrapper for clr bytecode.
newtype ClrBytecode = ClrBytecode
  { bytecode :: ByteString
  }

instance TH.Lift ClrBytecode where
  lift ClrBytecode{..} =
      [| ClrBytecode
           (BS.pack $(TH.lift (BS.unpack bytecode)))
       |]
mangleModule :: Module -> String
mangleModule (Module (PkgName pkg) (ModName m)) =
  printf "Inline__%s_%s" (filter isAlphaNum pkg) (map (\case '.' -> '_' ; x -> x) m)

-- | TH action that embeds bytecode in the current module via a top level
--   declaration of a StaticPtr
embedBytecode :: ClrBytecode -> Q ()
embedBytecode bs = do
    ptr <- TH.newName $ "_inlinejava__bytecode"
    TH.addTopDecls =<<
      sequence
        [ TH.sigD ptr [t| StaticPtr ClrBytecode |]
        , TH.valD (TH.varP ptr) (TH.normalB [| static $(TH.lift bs) |]) []
        ]

-- | Idempotent action that reads the embedded bytecodes in a module
--   by querying the table of static pointers
unembedBytecode :: IO ()
unembedBytecode = doit `seq` return ()
  where
    {-# NOINLINE doit #-}
    doit = unsafePerformIO $ do
      keys <- staticPtrKeys
      forM_ keys $ \key -> do
        unsafeLookupStaticPtr key >>= \case
          Just (sptr :: StaticPtr ClrBytecode) -> do
            let ClrBytecode bytes = deRefStaticPtr sptr
            loadBytecode bytes
          _ -> return ()

foreign import ccall "dynamic" assemblyLoad :: FunPtr (Ptr Int -> Int -> IO()) -> (Ptr Int -> Int -> IO ())

-- | Idempotent function that loads the bytecodes embedded in the static table for this module
loadBytecode :: ByteString -> IO ()
loadBytecode bs =
  getMethodStub
    "Salsa.Driver, Driver, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
    "LoadAssemblyFromBytes"
    "System.IntPtr;System.Int32" >>= \f ->
  BS.useAsCStringLen bs $ \(ptr,len) -> assemblyLoad f (castPtr ptr) len
