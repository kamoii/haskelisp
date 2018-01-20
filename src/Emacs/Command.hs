{-# LANGUAGE OverloadedStrings #-}
module Emacs.Command
 ( InteractiveForm(..)
 , setCommand
 ) where

import Prelude()
import Protolude hiding (Symbol)
import Emacs.Core
import Data.Text
import Control.Monad (void)

-- 依存型とか駆使すれば多分 interacvie form と関数の引数の型の整合性が
-- 取れていることが多分保証できる。

-- これは直接は使って欲くないため、' postfix を付けている。
-- EmacsValue を引数として取っているので function 以外のものを渡せてしまう
--
-- interactive-form はシンボル側及び関数側でも独立に情報を持つようだ。
-- 例えば (lambda (str) (interactive "s") (message str)) とか。
-- これはシンボル側の 属性設定からは影響を受けない。
-- 関数の interactive-form は作成後は変更できない模様(少なくとも関数はない)
--
--  * symbol から起動する場合はどっちかに interactive-form が設定されていれば OK
--    - 両方設定されている場合は symbol 側の interactive-form 属性が優先される模様
--    - 上記の挙動については `interactive-form` C関数のコード見れば、コメントで記載されている
--  * 直接関数オブジェクトから起動する場合は その関数側に interactive-form が設定されている必要がある。
--
-- (defun hoge (str) (interactive "s") ...) のように定義した場合は関
-- 数側に interactive-form が設定されるので、シンボル側の
-- interactive-form 属性は nil のまま。
--
-- > (get 'hoge 'interactive-form) ;=> nil
-- > (interactive-form 'hoge)      ;=> "s"
-- > (put  'hoge 'interactive-form '(interactive "s\ns\n"))  ;シンボル側に設定
-- > (interactive-form 'hoge)      ;=> "s\ns\n"   シンボル側優先

data InteractiveForm

setCommand
  :: Text
  -> InteractiveForm
  -> (TypedEmacsValue EmacsFunction)
  -> EmacsM ()
setCommand fname form f = do
  fnameQ <- intern fname
  interactiveFormQ <- intern "interactive-form"
  void $ call2 "fset" fnameQ f
  void $ call3 "put"  fnameQ interactiveFormQ =<< eval "'(interactive nil)"

