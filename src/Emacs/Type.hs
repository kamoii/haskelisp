{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Emacs.Type
  ( module Emacs.Type
  , CInt  -- for emacsModule
  ) where

import Prelude(Show(..))
import Protolude hiding (show)
import Data.IORef
import GHC.Ptr
import Foreign.C.Types
import Foreign.StablePtr
import Control.Monad.Reader

data PState = PState
  { symbolMap :: IORef (Map Text GlobalEmacsValue)
  }

data Ctx = Ctx
  { pstateStablePtr :: StablePtr PState
  , pstate :: PState
  , emacsEnv :: EmacsEnv
  }

type EmacsM =
  ReaderT Ctx IO

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
  = ESymbol
  | EInteger
  | EKeyword
  | EFunction
  | EString
  | ECons
  | EList
  | ENil
  | EBool
  | EKeymap
  | EKeyseq
  | EUnkown
  deriving (Show, Eq)

type EmacsModule = Ptr () -> IO CInt

newtype EmacsEnv   = EmacsEnv (Ptr ())
  deriving (Storable)

newtype EmacsValue = EmacsValue (Ptr ())
  deriving (Storable)

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
data EmacsKeyword  
data EmacsCons     
data EmacsFunction 
data EmacsList     
data EmacsNil      
data EmacsBool     
data EmacsKeymap   
data EmacsKeyseq   

-- Emacs の値に対する Haskell の型
-- 数値や文字列は素直なんだけど、他
-- Nil は空 [] でいいのかな？
newtype Symbol     = Symbol Text
newtype Keyword    = Keyword Text

-- 例外機構

data EmacsFuncallExit
  = EmacsFuncallExitReturn
  | EmacsFuncallExitSignal
  | EmacsFuncallExitThrow
  deriving (Show,Eq,Enum)

data EmacsException
  = EmacsException EmacsFuncallExit EmacsValue EmacsValue

instance Show EmacsException where
  show (EmacsException funcallExit _ _) =
    "EmacsException(" <> show funcallExit <> ")"

instance Exception EmacsException

-- 関数定義のため必要な型

type EFunctionStub
  = EmacsEnv
  -> CPtrdiff
  -> Ptr (Ptr ())
  -> StablePtr PState
  -> IO EmacsValue

data InteractiveForm = InteractiveNoArgs

newtype Doc   = Doc Text   -- ドキュメント(関数など)
newtype Arity = Arity Int  -- 関数アリティ
