{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Emacs.Keymap where

import Prelude()
import Protolude
import Emacs.Core
import Emacs.Symbol

instance CheckTypeEmacsValue 'EKeymap where
  typeCheck = typeCheckWithP "keymapp"

instance CheckTypeEmacsValue 'EKeyseq where
  typeCheck = undefined

getKeymap :: Text -> EmacsM EmacsKeymap
getKeymap name =
  join $ typeCheck <$> getValue name

-- とりあえずは一番簡単な方法で
-- kbd

kbd :: Text -> EmacsM EmacsKeyseq
kbd keys =
  TypedEmacsValue <$> funcall1 "kbd" keys
  

-- getKeymap :: Text -> EmacsM Keymap

-- そもそも設定できるのは何だ？
--
-- DEF is anything that can be a key’s definition:
--  nil (means key is undefined in this keymap),
--  a command (a Lisp function suitable for interactive calling),
--  a string (treated as a keyboard macro),
--  a keymap (to define a prefix key),
--  a symbol (when the key is looked up, the symbol will stand for its
--     function definition, which should at that time be one of the above,
--     or another symbol whose function definition is used, etc.),
--  a cons (STRING . DEFN), meaning that DEFN is the definition
--     (DEFN should be a valid definition in its own right),
--  or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
--  or an extended menu item definition.
--  (See info node ‘(elisp)Extended Menu Items’.)

-- Currentnly only Symbols
defineKey :: Text -> Symbol -> EmacsKeymap -> EmacsM ()
defineKey keyseq bindee keymap = do
  seq <- kbd keyseq
  void $ funcall3 "define-key" keymap seq bindee
