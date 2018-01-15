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
  call1 "mapatoms" (IOFn1 (accum ref))
  liftIO $ readIORef ref
  where
    accum :: IORef [EmacsValue] -> EmacsValue -> IO ()
    accum ref sym = modifyIORef ref (sym:)

-- Symbol has four slots.
--
--  1. name
--  2. value (* can have buffer local value)
--  3. function
--  4. property list (* can have buffer local list)

getSymbolName :: Text -> EmacsM Text
getSymbolName name = readEV =<< call1 "symbol-name" (Symbol name)

setValue :: WriteEmacsValue a => Text -> a -> EmacsM EmacsValue
setValue name val = call2 "set" (Symbol name) val

--  関数の設定
-- 一番 low level なのが setFunction
setFunction :: Text -> TypedEmacsValue EmacsFunction -> EmacsM ()
setFunction name f = void $ call2 "fset" (Symbol name) f

-- Could throw exception if the symbol is not setted.
getValue :: Text -> EmacsM EmacsValue
getValue name = call1 "symbol-value" (Symbol name)

-- if Symbol exists (included in obarray) and a value is bounded.
isBounded :: Text -> EmacsM Bool
isBounded name = readEV =<< call1 "boundp" (Symbol name)

-- Buffer local
--
-- If the variable is buffer local, you need to use
-- getDefaultValue/setDefaultValue to set/get the global variable.
getDefaultValue :: Text -> EmacsM EmacsValue
getDefaultValue name = call1 "default-value" (Symbol name)

setDefaultValue :: WriteEmacsValue a => Text -> a -> EmacsM EmacsValue
setDefaultValue name val = call2 "set-default" (Symbol name) val

--  Keyword Symbol

--  シンボルは任意の属性を持つことができる。
--
-- 属性テーブルは シンボルと任意の値に間のハッシュである。
-- ただし値として nil は設定できない。未設定とnil に設定は区別されない。
getProperty :: Text -> Text -> EmacsM (Maybe EmacsValue)
getProperty name property = do
  ev <- call2 "get" (Symbol name) (Symbol property)
  b <- not <$> isNil ev
  return $ if b then Just ev else Nothing

setProperty
  :: (WriteEmacsValue v)
  => Text
  -> Text
  -> v
  -> EmacsM EmacsValue
setProperty name property value =
  call3 "put" (Symbol name) (Symbol property) value
