{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.NAdvice where

import Prelude()
import Protolude hiding (Symbol)
import Emacs.Core
import Emacs.Class

-- Emacs 24 からアドバイスの機構が一新された(nadvice.el)。以前より大幅
-- にシンプルになっている。必要な関数は `advice-add` と
-- `advice-remove` の二つ。

-- (advice-add SYMBOL WHERE FUNCTION &optional PROPS)
--
-- Like ‘add-function’ but for the function named SYMBOL.
-- Contrary to ‘add-function’, this will properly handle the cases where SYMBOL
-- is defined as a macro, alias, command, ...
--
-- TODO: PROPSについては `add-function` のヘルプを参照

-- コマンドに advice が追加された場合はどのような挙動になるのだろうか...
--
-- 実際にやってみた。
-- インタラクティブに呼び出すと、まず先に interactive な挙動をした後、
-- non-interactively にアドバイスが呼ばれた。

-- 基本的に Arround 一つで全て実装できる。
--
--  `:before'       (lambda (&rest r) (apply FUNCTION r) (apply OLDFUN r))
--  `:after'        (lambda (&rest r) (prog1 (apply OLDFUN r) (apply FUNCTION r)))
--  `:around'       (lambda (&rest r) (apply FUNCTION OLDFUN r))
--  `:override'     (lambda (&rest r) (apply FUNCTION r))
--  `:before-while' (lambda (&rest r) (and (apply FUNCTION r) (apply OLDFUN r)))
--  `:before-until' (lambda (&rest r) (or  (apply FUNCTION r) (apply OLDFUN r)))
--  `:after-while'  (lambda (&rest r) (and (apply OLDFUN r) (apply FUNCTION r)))
--  `:after-until'  (lambda (&rest r) (or  (apply OLDFUN r) (apply FUNCTION r)))
--  `:filter-args'  (lambda (&rest r) (apply OLDFUN (funcall FUNCTION r)))
--  `:filter-return'(lambda (&rest r) (funcall FUNCTION (apply OLDFUN r)))
--
data Where
  = Around
  | Before
  | After
  | Override
  | BeforeWhile
  | BeforeUntil
  | AfterWhile
  | AfterUntil
  | FilterArgs
  | FIlterReturn

whereToKeyword :: Where -> Keyword
whereToKeyword Around = Keyword "around"
whereToKeyword Before = Keyword "before"

-- TODO: FUNCTION は シンボルでないと駄目？ -> いや、関数でもOK
-- 存在しないシンボルに対しても設定できる。
-- アドバイス関数には 第一引数に元関数、残りは引数が渡される。
-- 引数を (f &rest args) のように受けると、 (apply f args) で元関数を適用可能。
adviceAdd
  :: Text
  -> Where
  -> CallableEmacsValue
  -> EmacsM ()
adviceAdd target where' cev = do
  targetEv <- intern target
  whereEv <- mkKeyword $ whereToKeyword where'
  void $ call3 "advice-add" targetEv whereEv cev

-- 基本的にこの関数さえあれば何でもできる。
-- TODO: アドバイス外せるように
around''
  :: Text
  -> (TypedEmacsValue EmacsFunction -> [EmacsValue] -> EmacsM EmacsValue)
  -> EmacsM ()
around'' name ff = do
  fun <- mkFun (Doc "") (Arity 0,Arity 1000) (\(f:args) -> ff (unsafeType f) args)
  adviceAdd name Around (CEVFunction fun)

-- 第一引数として元の関数適用を行なうアクション、第二引数として引数を受け取る
-- 元の関数は基本的に素直に呼び出すことが殆んどなので。
around'
  :: Text
  -> (EmacsM EmacsValue -> [EmacsValue] -> EmacsM EmacsValue)
  -> EmacsM ()
around' name ff =
  around'' name $ \f args -> ff (call' (CEVFunction f) args) args

around
  :: EmacsFun a
  => Text
  -> (EmacsM EmacsValue -> a)
  -> EmacsM ()
around name ff = around' name (emacsFun <$> ff)
