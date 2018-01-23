{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings, TypeApplications, DataKinds #-}
module Main where

import Prelude()
import Protolude
import Emacs.Core
import Emacs.Class

foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

emacsModuleInit :: EmacsModule
emacsModuleInit = emacsModule (Just "core-test") $ do
  setValue "core-test-foo" =<< mkInteger 42
  setValue "core-test-bar" =<< mkString "日本語も問題ないはず"
  setFunction "core-test-append" =<< mkIOFun1 (readString. unsafeType) (pure . untype)
    (\str -> do
        mkString $ str <> " yeah!"
    )
  
  defvar "yeah" ("簡単に定義だ!" :: Text)
  defun "append-foo" $ \str -> str <> ("-foo" :: Text)
  defcmd "hello" InteractiveNoArgs $ do
    call1 "message" =<< mkString "world"

main :: IO ()
main = undefined
