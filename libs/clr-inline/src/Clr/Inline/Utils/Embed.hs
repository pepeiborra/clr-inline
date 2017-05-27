{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TemplateHaskell     #-}
module Clr.Inline.Utils.Embed where

import           Clr.Host.DriverEntryPoints
import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Foreign
import           GHC.StaticPtr
import           Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax as TH
import           System.IO.Unsafe

-- | A wrapper for clr bytecode.
newtype ClrBytecode = ClrBytecode
  { bytecode :: ByteString
  }

instance TH.Lift ClrBytecode where
  lift ClrBytecode{..} =
      [| ClrBytecode
           (BS.pack $(TH.lift (BS.unpack bytecode)))
       |]

-- | TH action that embeds bytecode in the current module via a top level
--   declaration of a StaticPtr
embedBytecode :: String -> ClrBytecode -> Q ()
embedBytecode name bs = do
    ptr <- TH.newName $ name ++ "_inlineclr__bytecode"
    TH.addTopDecls =<<
      sequence
        [ TH.sigD ptr [t| StaticPtr ClrBytecode |]
        , TH.valD (TH.varP ptr) (TH.normalB [| static $(TH.lift bs) |]) []
        ]

-- | Idempotent action that reads the embedded bytecodes in a module
--   by querying the table of static pointers
unembedBytecode :: IO ()
{-# NOINLINE unembedBytecode #-}
unembedBytecode = doit `seq` return ()
  where
    {-# NOINLINE doit #-}
    doit = unsafePerformIO $ do
      keys <- staticPtrKeys
      forM_ keys $
        unsafeLookupStaticPtr >=> \case
          Just (sptr :: StaticPtr ClrBytecode) -> do
            let ClrBytecode bytes = deRefStaticPtr sptr
            loadBytecode bytes
          _ -> return ()

foreign import ccall "dynamic" assemblyLoad :: FunPtr (Ptr Int -> Int -> IO()) -> (Ptr Int -> Int -> IO ())

-- | Idempotent function that loads the bytecodes embedded in the static table for this module
loadBytecode :: ByteString -> IO ()
loadBytecode bs =
  unsafeGetPointerToMethod "LoadAssemblyFromBytes" >>= \f ->
  BS.useAsCStringLen bs $ \(ptr,len) -> assemblyLoad f (castPtr ptr) len
