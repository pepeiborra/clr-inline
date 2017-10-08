{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeInType             #-}
module Clr.Inline.Types
  ( ClrPtr(..)
  , Clr(..)
  , ClrType(Con, Unit, ..)
  , ClrTypeSymbol(..)
  , renderClrType
  , TextBStr(..)
  ) where

import           Clr.Host.GCHandle
import           Clr.Host.BStr
import           Clr.Marshal
import           Data.Coerce
import           Data.List
import           Data.Text            (Text)
import           Foreign
import           GHC.TypeLits
import           System.IO.Unsafe

data ClrType = TyCon String [ClrType]
             | Fun [ClrType] ClrType
             | Array Int ClrType
  deriving (Eq, Show)

renderClrType :: ClrType -> String
renderClrType = show where
  show (Array dim x) = concat [show x, "[", replicate (pred dim) ',', "]"]
  show (TyCon con []) = con
  show (TyCon con args) = concat [con,"<",intercalate "," (map show args),">"]
  show (Fun args res) = intercalate "->" $ map show (args ++ [res])

pattern Unit :: ClrType
pattern Unit = TyCon "unit" []
pattern Con :: String -> ClrType
pattern Con x = TyCon x []

-- | A type indexed pointer to a Clr object.
newtype ClrPtr (name::Symbol)= ClrPtr (GCHandle Int)

-- | A type indexed pointer to a Clr object with GC managed lifetime.
newtype Clr (name::Symbol) = Clr (ForeignPtr Int)

-- Returning from a CLR function that was called from Haskell
instance Unmarshal (ClrPtr n) (Clr n) where
  unmarshal (ClrPtr id) = do
    ptr <- newForeignPtr (unsafeDupablePerformIO gcHandleFinalizer) (coerce id)
    return (Clr ptr)

-- Returning from a Haskell function that was called by the CLR
instance {-# OVERLAPPING #-} Unmarshal (Clr t) (ClrPtr t) where
  unmarshal (Clr x) = withForeignPtr x $ \x'-> ClrPtr <$> newHandle (coerce x')

-- Calling a CLR function from Haskell
instance Marshal (Clr n) (ClrPtr n) where
  marshal (Clr ptr) f = withForeignPtr ptr $ \p -> f (ClrPtr $ coerce p)

-- Calling a Haskell function from the CLR
instance {-# OVERLAPPING #-} Marshal (ClrPtr t) (Clr t) where
  marshal (ClrPtr x) f = do
    x' <- newHandle x
    fp <- newForeignPtr (unsafeDupablePerformIO gcHandleFinalizer) (coerce x')
    f $ Clr fp

newtype ClrTypeSymbol = ClrTypeSymbol {getClrTypeSymbol :: String} deriving Show

newtype TextBStr = TextBStr BStr
instance Unmarshal TextBStr Text where unmarshal (TextBStr t) = unmarshal t
instance Marshal Text TextBStr where marshal x f = marshal x (f . TextBStr)

