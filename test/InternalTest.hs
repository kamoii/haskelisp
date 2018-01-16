{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
module Main where

import Prelude()
import Protolude
import Foreign.C.Types (CInt(..))
import Emacs.Internal  

foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

emacsModuleInit :: EmacsModule
emacsModuleInit = emacsModule $ \env -> do
  str <- makeString env "this is a simple test"
  sym <- intern env "message"
  funcall env (untype sym) [untype str]
  pure ()

main :: IO ()
main = undefined
