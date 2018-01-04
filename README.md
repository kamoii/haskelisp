EXPERIMENTAL

Write Emacs module in Haskell, using Emacs 25's Dynamic Module feature.

* Only tested with linux.
* Only tested with Stack (LTS 6.26)
* You need to build emacs with --with-modules configuration options
* You need to specify some ghc-options to make it work

Sample:

    {-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
    module Main where

    import Emacs
    import Foreign.C.Types

    foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

    emacsModuleInit :: EmacsModule
    emacsModuleInit = defmodule "sample-module" $ do

      setVal "foo" (Symbol "bar")

      defun "square" $ \i -> do
        message "haskell squre function called"
        return $ (i*i :: Int)

    main :: IO ()
    main = undefined

# How to use

Explaining using Stack and LTS 6.26 as premise.

## 1. Create a new project with Stack

    $ stack --resolver=lts-6.26 new mymodule

## 2. Change executable name to *.so and add haskelisp to the dependency

mymodule.cabal:

    executable mymodule.so
      hs-source-dirs:      app
      main-is:             Main.hs
      ghc-options:         -threaded -rtsopts -with-rtsopts=-N
      build-depends:       base
                         , mymodule
                         , haskelisp
      default-language:    Haskell2010

## 3. Change `ghc-options` and add `cc-options` to make shared library

mymodule.cabal:

    executable mymodule.so
      ...
      cc-options:          -fPIC
      ghc-options:         -shared -dynamic -fPIC -lHSrts-ghc7.10.3
      ...

## 4. Modules must be GPL compatible

The shared library must include `plugin_is_GPL_compatible` symbol to be loaded by Emacs.
Prepare a C source file and specify it with `c-sources` option.

    $ echo 'int plugin_is_GPL_compatible;' > plugin_is_GPL_compatible.c

mymodule.cabal:

    executable mymodule.so
      ...
      c-sources:           plugin_is_GPL_compatible.c
      ...

## 5. Write some code

Main.hs:

    {-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
    module Main where

    import Emacs
    import Foreign.C.Types

    foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

    emacsModuleInit :: EmacsModule
    emacsModuleInit = defmodule "mymodule" $ do

      defun "mysquare" $ \i -> do
        message "haskell squre function called"
        return $ (i*i :: Int)

    main :: IO ()
    main = undefined

We don't need `main` function, but without it cause a compile error,
so include a dummy one. It won't be called.

## 6. Build

    $ stack build

## 7. Copy the genereated shared library under `load-path`

For example, if `~/.emacs.d/lisp` is included in `load-path`:

    $ cp .stack-work/install/x86_64-linux/lts-6.26/7.10.3/bin/mymodule.so ~/.emacs.d/lisp/

## 8. Load your shared library from Emacs

    (require 'mymodule)
