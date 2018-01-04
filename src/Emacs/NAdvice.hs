{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.NAdvice where

import Prelude()
import Protolude
import Emacs.Core

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
adviceAdd'
  :: (ToEmacsSymbol s, ToEmacsFunction f)
  => s
  -> Where
  -> f
  -> EmacsM ()
adviceAdd' target where' func =
  void $ funcall3 "advice-add" target (whereToKeyword where') func

-- 基本的にこの関数さえあれば何でもできる。
-- TODO: アドバイス外せるように
around' :: Functionable f => Text -> (EmacsFunction -> f) -> EmacsM ()
around' name ff = do
  adviceAdd' (Symbol name) Around ff

-- aroundアドバイスの場合、大抵は引数は弄らない。ショートカット的。
-- TODO:
around :: Functionable f => Text -> (EmacsM EmacsValue -> f) -> EmacsM ()
around name ff = do
  adviceAdd' (Symbol name) Around =<< wrap ff
  where
    wrap :: Functionable f => (EmacsM EmacsValue -> f) -> EmacsM EmacsFunction
    wrap newf =
      let wf :: [EmacsValue] -> EmacsM EmacsValue
          wf (func:args) = do
            res <- call (newf (funcall func args)) args
            case res of
              Right ev -> return ev
              Left   _ -> undefined
      in mkFunction wf 0 1000 "around advice"
