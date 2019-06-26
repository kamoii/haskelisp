EXPERIMENTAL

Write Emacs module in Haskell, using Emacs 25's Dynamic Module feature.

* Only tested with linux.
* Only tested with Stack (LTS 6.26)
* You need to build emacs with --with-modules configuration options
* You need to specify some ghc-options to make it work

Sample:

    {-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
    module Main where

    import Emacs.Core
    import Foreign.C.Types

    foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

    emacsModuleInit :: EmacsModule
    emacsModuleInit = emacsModule (Just "mymodule") $ do

      fun <- mkIOFun1 (readString . unsafeType) (fmap untype . mkString) $ \txt -> do
        message $ "haskell squre function called: " <> txt
        pure $ "!" <> txt <> "!"

      setCommand "great-from-haskell" InteractiveNoArgs fun

    main :: IO ()
    main = undefined

# How to use

Explaining using Stack and LTS 13.26 as premise.

## 1. Create a new project with Stack

    $ stack --resolver=lts-13.26 new mymodule

## 2. Change executable name to *.so and add haskelisp to the dependency

project.yaml:

    executables:
      mymodule.so:
        main:                Main.hs
        source-dirs:         app
        ghc-options:
        - -threaded
        - -rtsopts
        - -with-rtsopts=-N
        dependencies:
        - mymodule
        - haskelisp

## 3. Change `ghc-options` and add `cc-options` to make shared library

project.yaml:

    executables:
      mymodule.so:
        main:                Main.hs
        source-dirs:         app
        cc-options:          -fPIC
        ghc-options:
        - -shared
        - -dynamic
        - -fPIC
        - -lHSrts-ghc8.6.5

## 4. Modules must be GPL compatible

The shared library must include `plugin_is_GPL_compatible` symbol to be loaded by Emacs.
Prepare a C source file and specify it with `c-sources` option.

    $ echo 'int plugin_is_GPL_compatible;' > plugin_is_GPL_compatible.c

project.yaml:

    executables:
      mymodule.so:
        ...
        c-sources:           plugin_is_GPL_compatible.c

## 5. Write some code

Main.hs:

    {-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
    module Main where

    import Emacs.Core
    import Foreign.C.Types

    foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

    emacsModuleInit :: EmacsModule
    emacsModuleInit = emacsModule (Just "mymodule") $ do

      fun <- mkIOFun1 (readString . unsafeType) (fmap untype . mkString) $ \txt -> do
        message $ "haskell squre function called: " <> txt
        pure $ "!" <> txt <> "!"

      setCommand "great-from-haskell" InteractiveNoArgs fun

    main :: IO ()
    main = undefined

We don't need `main` function, but without it cause a compile error,
so include a dummy one. It won't be called.

## 6. Build

    $ stack build

## 7. Copy the genereated shared library under `load-path`

For example, if `~/.emacs.d/lisp` is included in `load-path`:

    $ cp $(stk path --local-install-root)/bin/mymodule.so ~/.emacs.d/lisp/

Or you can start a clean emacs with load-path seted:

    $ emacs --no-splash -q -L $(stk path --local-install-root)/bin/

## 8. Load your shared library from Emacs

    (require 'mymodule)
