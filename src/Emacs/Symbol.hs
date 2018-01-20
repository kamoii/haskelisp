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
  fun <- mkIOFun1 pure (const mkNil) $ accum ref
  call1 "mapatoms" fun
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
getSymbolName name = do
  name <- call1 "symbol-name" =<< intern name
  readString $ unsafeType name

-- Could throw exception if the symbol is not setted.
getValue :: Text -> EmacsM EmacsValue
getValue name =
  call1 "symbol-value" =<< intern name

-- if Symbol exists (included in obarray) and a value is bounded.
isBounded :: Text -> EmacsM Bool
isBounded name =
  readBool =<< call1 "boundp" =<< intern name

-- Buffer local
--
-- If the variable is buffer local, you need to use
-- getDefaultValue/setDefaultValue to set/get the global variable.
getDefaultValue :: Text -> EmacsM EmacsValue
getDefaultValue name =
  call1 "default-value" =<< intern name

setDefaultValue :: IsEmacsValue a => Text -> a -> EmacsM EmacsValue
setDefaultValue name val = do
  nameQ <- intern name
  call2 "set-default" nameQ val

--  Keyword Symbol

--  シンボルは任意の属性を持つことができる。
--
-- 属性テーブルは シンボルと任意の値に間のハッシュである。
-- ただし値として nil は設定できない。未設定とnil に設定は区別されない。
getProperty :: Text -> Text -> EmacsM (Maybe EmacsValue)
getProperty name property = do
  nameQ <- intern name
  propertyQ <- intern property
  ev <- call2 "get" nameQ propertyQ
  b <- not <$> isNil ev
  return $ if b then Just ev else Nothing

setProperty
  :: (IsEmacsValue v)
  => Text
  -> Text
  -> v
  -> EmacsM EmacsValue
setProperty name property value = do
  nameQ <- intern name
  propertyQ <- intern property
  call3 "put" nameQ propertyQ value
