{-# LANGUAGE OverloadedStrings #-}

module Emacs.Module
  ( evalAfterLoad
  ) where

import Prelude()
import Protolude
import Emacs.Core

evalAfterLoad :: Text -> EmacsM a -> EmacsM ()
evalAfterLoad name prog = 
  void $ funcall2 "eval-after-load" (Symbol name) (Function (void prog))
