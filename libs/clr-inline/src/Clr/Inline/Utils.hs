module Clr.Inline.Utils where

import           Control.Monad.Trans.Writer

yield :: Monad m => t -> WriterT [t] m ()
yield x = tell [x]
yieldAll :: Monad m => w -> WriterT w m ()
yieldAll = tell
