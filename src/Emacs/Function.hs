{-# LANGUAGE OverloadedStrings #-}

module Emacs.Function where

import Prelude()
import Protolude
import Emacs.Core

--  関数の設定
-- 一番 low level なのが setFunction
setFunction :: Text -> EmacsFunction -> EmacsM ()
setFunction name f = do
  void $ funcall2 "fset" (Symbol name) f

--  より elisp に近い形で記述したいのであればこちら
defun' :: Text -> Doc -> Arity -> ([EmacsValue] -> EmacsM EmacsValue) -> EmacsM ()
defun' name (Doc doc) (Arity arity) f =
  setFunction name =<< mkFunction f arity arity doc

defun :: Functionable f => Text -> f -> EmacsM ()
defun name f =
  setFunction name =<< mkFunctionFromFunctionable f
