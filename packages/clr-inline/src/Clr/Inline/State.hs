{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Clr.Inline.State where

import Control.Monad
import Data.Typeable
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

data FinalizerState a = FinalizerState
  { finalizerCount :: Int
  , wrappers :: [a]
  }

initialFinalizerState :: FinalizerState a
initialFinalizerState = FinalizerState 0 []

getFinalizerState :: forall a . Typeable a => Q (FinalizerState a)
getFinalizerState = TH.getQ >>= \case
    Nothing -> do
      TH.putQ (initialFinalizerState @ a)
      return initialFinalizerState
    Just st -> return st

setFinalizerState :: Typeable a => FinalizerState a -> Q ()
setFinalizerState = TH.putQ

incrementFinalizerCount :: forall a. Typeable a => Q ()
incrementFinalizerCount =
    getFinalizerState @ a >>= \FinalizerState{..} ->
    setFinalizerState FinalizerState{finalizerCount = finalizerCount + 1, ..}

decrementFinalizerCount :: forall a . Typeable a => Q ()
decrementFinalizerCount =
    getFinalizerState @ a >>= \FinalizerState{..} ->
    setFinalizerState FinalizerState{finalizerCount = max 0 (finalizerCount - 1), ..}

isLastFinalizer :: forall a . Typeable a => Q Bool
isLastFinalizer =
  getFinalizerState @a >>= \FinalizerState {..} -> return $ finalizerCount == 0

pushWrapper :: forall a . Typeable a => a -> Q ()
pushWrapper w =
    getFinalizerState @ a >>= \FinalizerState{..} ->
    setFinalizerState FinalizerState{wrappers = w:wrappers, ..}

pushWrapperGen :: forall a . Typeable a => (Q ()) -> Q a -> Q ()
pushWrapperGen actionIfLast gen = do
    incrementFinalizerCount @ a
    TH.addModFinalizer $ do
      decrementFinalizerCount @ a
      pushWrapper =<< gen
      isLast <- isLastFinalizer @ a
      when isLast actionIfLast
