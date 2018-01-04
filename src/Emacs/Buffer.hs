{-# LANGUAGE OverloadedStrings #-}
module Emacs.Buffer where

import Control.Monad (void)
import Emacs.Core
import Data.Text

insertToCurrentBuffer :: Text -> EmacsM ()
insertToCurrentBuffer t =
  void $ funcall1 "insert" t
