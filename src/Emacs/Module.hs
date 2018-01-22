{-# LANGUAGE OverloadedStrings #-}

module Emacs.Module
  ( evalAfterLoad
  ) where

import Prelude()
import Protolude
import Emacs.Core

evalAfterLoad :: Text -> EmacsM () -> EmacsM ()
evalAfterLoad name prog = do
  nameEv <- intern name
  void $ call2 "eval-after-load" nameEv (Function (void prog))
