{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
module Main where

import Prelude()
import Protolude
import Emacs.Core

foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

emacsModuleInit :: EmacsModule
emacsModuleInit = emacsModule (Just "core-test") $ do
  setValue "core-test-foo" =<< mkInteger 42
  setValue "core-test-bar" =<< mkString "日本語も問題ないはず"
  setFunction "core-test-append" =<< mkIOFun1 (readString. unsafeType) (pure . untype)
    (\str -> do
        mkString $ str <> " yeah!"
    )
  pure ()

main :: IO ()
main = undefined
