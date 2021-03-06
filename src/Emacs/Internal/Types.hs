{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.Internal.Types
  ( module Emacs.Internal.Types
  , CInt
  ) where

import Prelude(Show(..))
import Protolude hiding (show)
import Data.IORef
import GHC.Ptr
import Foreign.C.Types
import Foreign.StablePtr

-- nil について
--
-- emacs 内部では nil は文字列で表現できないシンボルとして定義されている。
-- globals.h
--
-- #define Qnil builtin_lisp_symbol (0)
--
-- type-of では取得できない。nil は型ではない？かな。多分 type_of に渡
-- すとエラーになる。
--
-- haskell では不便なので ENil という型を導入する。
data EmacsType
  = ETSymbol
  | ETInteger
  | ETKeyword
  | ETFunction
  | ETString
  | ETCons
  | ETList
  | ETNil
  | ETBool
  | ETKeymap
  | ETKeyseq
  | ETUnkown
  deriving (Show, Eq)

type EmacsModule = EmacsRuntime -> IO CInt

newtype EmacsRuntime = EmacsRuntime (Ptr ())
  deriving (Storable)

newtype EmacsEnv   = EmacsEnv (Ptr ())
  deriving (Storable)

newtype EmacsValue = EmacsValue (Ptr ())
  deriving (Storable)

-- `make_global_re`関数で
newtype GlobalEmacsValue = GlobalEmacsValue (Ptr ())
  deriving (Storable)

castGlobalToEmacsValue
  :: GlobalEmacsValue
  -> EmacsValue
castGlobalToEmacsValue (GlobalEmacsValue p) =
  EmacsValue p

newtype TypedEmacsValue et = TypedEmacsValue EmacsValue

untype
  :: TypedEmacsValue et
  -> EmacsValue
untype (TypedEmacsValue ev) = ev

unsafeType
  :: EmacsValue
  -> TypedEmacsValue et
unsafeType = TypedEmacsValue

-- EmacsValue Opaque Type :w
data EmacsSymbol   
data EmacsInteger  
data EmacsString   
data EmacsFunction 

-- 例外機構

data EmacsFuncallExit
  = EmacsFuncallExitReturn
  | EmacsFuncallExitSignal
  | EmacsFuncallExitThrow
  deriving (Show,Eq,Enum)

-- Emacs側で発生した例外を表現
data EmacsException
  = EmacsException EmacsFuncallExit EmacsValue EmacsValue

instance Show EmacsException where
  show (EmacsException funcallExit _ _) =
    "EmacsException(" <> show funcallExit <> ")"

instance Exception EmacsException

-- 関数定義のため必要な型

type EFunctionStub a
  = EmacsEnv
  -> CPtrdiff
  -> Ptr (Ptr ())
  -> StablePtr a
  -> IO EmacsValue

newtype Doc   = Doc Text   -- ドキュメント(関数など)
newtype Arity = Arity Int  -- 関数アリティ
