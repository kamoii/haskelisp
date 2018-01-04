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
  -- funcall1 "mapatoms" =<< mkFunctionFromFunctionable (accum ref)
  funcall1 "mapatoms" (accum ref)
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
getSymbolName name =
  extractString . typeUnsafe =<< funcall1 "symbol-name" (Symbol name)

setValue :: ToEmacsValue a => Text -> a -> EmacsM EmacsValue
setValue name val =
  funcall2 "set" (Symbol name) val

-- Could throw exception if the symbol is not setted.
getValue :: Text -> EmacsM EmacsValue
getValue name =
  funcall1 "symbol-value" (Symbol name)

-- helper function
getValueAs :: FromEmacsValue t => Text -> EmacsM t
getValueAs name = fromEv =<< getValue name

-- if Symbol exists (included in obarray) and a value is bounded.
isBounded :: Text -> EmacsM Bool
isBounded name =
  isNotNil =<< funcall1 "boundp" (Symbol name)

-- Buffer local
--
-- If the variable is buffer local, you need to use
-- getDefaultValue/setDefaultValue to set/get the global variable.
getDefaultValue :: Text -> EmacsM EmacsValue
getDefaultValue name =
  funcall1 "default-value" (Symbol name)

setDefaultValue :: ToEmacsValue a => Text -> a -> EmacsM EmacsValue
setDefaultValue name val =
  funcall2 "set-default" (Symbol name) val

--  Keyword Symbol

--  シンボルは任意の属性を持つことができる。
--
-- 属性テーブルは シンボルと任意の値に間のハッシュである。
-- ただし値として nil は設定できない。未設定とnil に設定は区別されない。
getProperty :: Text -> Text -> EmacsM (Maybe EmacsValue)
getProperty name property = do
  ev <- funcall2 "get" (Symbol name) (Symbol property)
  b <- isNotNil ev
  return $ if b then Just ev else Nothing

setProperty
  :: (ToEmacsValue v)
  => Text
  -> Text
  -> v
  -> EmacsM EmacsValue
setProperty name property value =
  funcall3 "put" (Symbol name) (Symbol property) value
