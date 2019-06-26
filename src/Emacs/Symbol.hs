{-# LANGUAGE OverloadedStrings #-}
module Emacs.Symbol where

import Prelude()
import Protolude
import Emacs.Core
import Data.IORef

-- All symbols
--
-- obarray に設定されている全てのシンボルを取得する。
-- Use `mapatoms` functoin.
allSymbols :: EmacsM [EmacsValue]
allSymbols = do
  ref <- liftIO $ newIORef []
  fun <- mkIOFun1 pure (const mkNil) $ accum ref
  call1 "mapatoms" fun
  liftIO $ readIORef ref
  where
    accum :: IORef [EmacsValue] -> EmacsValue -> IO ()
    accum ref sym = modifyIORef ref (sym:)
