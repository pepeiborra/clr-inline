{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Clr.Inline.IEnumerable where

import Clr.Inline
import Control.Monad
import Pipes

[fsharp|open System.Collections|]

toProducer :: Clr "IEnumerable" -> Producer (Clr "obj") IO ()
toProducer enumerable = do
  enumerator <- liftIO [fsharp| IEnumerator{ ($enumerable:IEnumerable).GetEnumerator() } |]
  let loop = do
        nxt <- liftIO [fsharp| bool{ ($enumerator:IEnumerator).MoveNext()} |]
        when nxt $ do
            cur <- liftIO [fsharp| obj{ ($enumerator:IEnumerator).Current } |]
            yield cur
            loop
  loop
