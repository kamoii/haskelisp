{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Emacs.Class
  ( EmacsFun
  , FunType(..)
  , toEmacsFun
  , funWithDoc
  , fun
  , ToEmacsValue(..)
  , UnsafeReadEmacsValue(..)
  , defvar
  , defun, defunWithDoc
  , defcmd, defcmdWithDoc
  ) where

import Prelude()
import Protolude hiding (Symbol)
import Emacs.Core
import GHC.TypeLits hiding (Symbol)

-- Nat の導入で Overlapping の問題を解決することができる。
-- ただし ghc は停止性に関しては保証ができないので UndecidableInstances 拡張が必要。
-- smaller head エラーが出る。Nat = Zero | Succ Nat 方式だと出ない(ghcが停止性保証できるので)
-- 参照
-- http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html  
--
-- Proxy 面倒。AllowAmbiguousTypes + TypeApplicaton でどうだ？
-- これいいのかな？すっきりするが、ちょっと怖い。
--
-- うーん、Overlapping と比べてどちらがいいんだろうか？
--
-- 使う側でも TypeApplications と DataKind が必要。

data FunType = PureFun | IOFun

class EmacsFun (ft :: FunType) (n :: Nat) a where
  toEmacsFun' :: a -> [EmacsValue] -> EmacsM  (Either Text EmacsValue)

instance ToEmacsValue a => EmacsFun PureFun 0 a where
  toEmacsFun' a [] = Right <$> toEv a

instance ToEmacsValue a => EmacsFun IOFun 0 (IO a) where
  toEmacsFun' a [] = do
    v <- liftIO a
    Right <$> toEv v

instance ToEmacsValue a => EmacsFun IOFun 0 (EmacsM a) where
  toEmacsFun' a [] = do
    v <- a
    Right <$> toEv v

instance (UnsafeReadEmacsValue a, EmacsFun ft (n-1) b) => EmacsFun ft n (a -> b) where
  toEmacsFun' f (ev:evs) = do
    a <- unsafeReadEv ev
    toEmacsFun' @ft @(n-1) (f a) evs

-- 外から typeApplication で指定してもらうために forall ft n a が必要。  
toEmacsFun
  :: forall ft n a
   . (EmacsFun ft n a, KnownNat n)
  => a
  -> [EmacsValue]
  -> EmacsM EmacsValue  
toEmacsFun a evs = do
  let argNum = natVal (Proxy :: Proxy n)
  when (fromInteger argNum /= length evs)
    $ throwIO $ ErrorCall "Indifferent arg num."
  toEmacsFun' @ft @n a evs >>= \case
    Left t -> throwIO $ ErrorCall $ toS t
    Right v -> pure v

funWithDoc 
  :: forall ft n a
   . (EmacsFun ft n a, KnownNat n)
  => Doc
  -> a
  -> EmacsM (TypedEmacsValue EmacsFunction)
funWithDoc doc a = do  
  let arity = fromInteger $ natVal (Proxy :: Proxy n)
  mkFun doc (Arity arity, Arity arity) (toEmacsFun @ft @n a)

fun
  :: forall ft n a
   . (EmacsFun ft n a, KnownNat n)
  => a
  -> EmacsM (TypedEmacsValue EmacsFunction)
fun = funWithDoc @ft @n (Doc "")  

-- * ToEmacsValue/UnsafeReadEmacsValue

class ToEmacsValue a where
  toEv :: a -> EmacsM EmacsValue

class UnsafeReadEmacsValue a where
  unsafeReadEv :: EmacsValue -> EmacsM a

newtype ReadEmacsValueException = ReadEmacsValueException Text
  deriving (Show)
instance Exception ReadEmacsValueException

instance ToEmacsValue EmacsValue where
  toEv = pure 

instance UnsafeReadEmacsValue EmacsValue where
  unsafeReadEv = pure

instance ToEmacsValue (TypedEmacsValue t) where
  toEv tev = pure $ untype tev

unsafeReadEvToTypedEv typef tname ev = do
  typef ev >>= \case
    Nothing -> throwIO $ ReadEmacsValueException $ "not " <> tname
    Just v -> pure v

instance UnsafeReadEmacsValue (TypedEmacsValue EmacsSymbol) where
  unsafeReadEv = unsafeReadEvToTypedEv typeSymbol "symbol"

instance ToEmacsValue Symbol where
  toEv sym = untype <$> mkSymbol sym

instance UnsafeReadEmacsValue Symbol where
  unsafeReadEv = readSymbol . unsafeType

instance ToEmacsValue Text where
  toEv txt = untype <$> mkString txt

instance UnsafeReadEmacsValue Text where
  unsafeReadEv = readString . unsafeType
    
instance ToEmacsValue () where
  toEv = const mkNil                     

instance UnsafeReadEmacsValue () where
  unsafeReadEv ev = do
    whenM (not <$> isNil ev) $ throwIO $ ReadEmacsValueException "not nil"
    pure ()

instance ToEmacsValue Bool where
  toEv = mkBool

instance UnsafeReadEmacsValue Bool where
  unsafeReadEv = readBool

instance ToEmacsValue a => ToEmacsValue [a] where
  toEv as = traverse toEv as >>= mkList

instance UnsafeReadEmacsValue a => UnsafeReadEmacsValue [a] where   
  unsafeReadEv ev = traverse unsafeReadEv =<< unsafeReadList ev

instance ToEmacsValue a => ToEmacsValue (Maybe a) where
  toEv Nothing = mkNil
  toEv (Just a) = toEv a

instance UnsafeReadEmacsValue a => UnsafeReadEmacsValue (Maybe a) where
  unsafeReadEv ev =
    ifM (isNil ev) (pure Nothing) (Just <$> unsafeReadEv ev)

-- * def系
-- ただ Emacs の def とは意味あいが違うことに注意。

defvar
  :: ToEmacsValue ev
  => Text
  -> ev
  -> EmacsM ()
defvar name ev =
  void $ setValue name =<< toEv ev  

defunWithDoc 
  :: forall ft n a
   . (EmacsFun ft n a, KnownNat n)
  => Text
  -> Doc
  -> a
  -> EmacsM ()
defunWithDoc name doc a = 
  setFunction name =<< funWithDoc @ft @n doc a

defun
  :: forall ft n a
   . (EmacsFun ft n a, KnownNat n)
  => Text
  -> a
  -> EmacsM ()
defun name = defunWithDoc @ft @n name (Doc "")  

defcmdWithDoc 
  :: forall ft n a
   . (EmacsFun ft n a, KnownNat n)
  => Text
  -> Doc
  -> InteractiveForm
  -> a
  -> EmacsM ()
defcmdWithDoc name doc iform a = 
  setCommand name iform =<< funWithDoc @ft @n doc a

defcmd
  :: forall ft n a
   . (EmacsFun ft n a, KnownNat n)
  => Text
  -> InteractiveForm
  -> a
  -> EmacsM ()
defcmd name = defcmdWithDoc @ft @n name (Doc "")
  
-- -- * funcall: 関数呼び出しの効率化
-- 
-- -- Symbol or a Function
-- -- ただ シンボルに functoin が設定されているかどうかまでは確認しない。
-- -- Text も特別に便宜のため対応させる -> OverloadedString と相性が悪いので止め
-- class WriteEmacsValue c => ToEmacsCallable c where
--   toCallableEmacsValue :: c -> EmacsM EmacsValue
--   toCallableEmacsValue = writeEV
-- 
-- instance ToEmacsCallable Symbol
-- instance ToEmacsCallable (TypedEmacsValue EmacsSymbol)
-- instance ToEmacsCallable (TypedEmacsValue EmacsFunction)
-- 
-- call0'
--   :: (ToEmacsCallable f)
--   => f
--   -> EmacsM EmacsValue
-- call0' f = do
--   apply' f []
-- 
-- call0 fname = call0' (Symbol fname)
-- 
-- call1'
--   :: (ToEmacsCallable f, WriteEmacsValue a)
--   => f
--   -> a
--   -> EmacsM EmacsValue
-- call1' f ev0 = do
--   args <- sequence [writeEV ev0]
--   apply' f args
-- 
-- call1 fname = call1' (Symbol fname)
-- 
-- call2'
--   :: (ToEmacsCallable f, WriteEmacsValue a, WriteEmacsValue b)
--   => f
--   -> a
--   -> b
--   -> EmacsM EmacsValue
-- call2' f ev0 ev1 = do
--   args <- sequence [writeEV ev0, writeEV ev1]
--   apply' f args
-- 
-- call2 fname = call2' (Symbol fname)
-- 
-- call3'
--   :: (ToEmacsCallable f, WriteEmacsValue a, WriteEmacsValue b, WriteEmacsValue c)
--   => f
--   -> a
--   -> b
--   -> c
--   -> EmacsM EmacsValue
-- call3' f ev0 ev1 ev2 = do
--   args <- sequence [writeEV ev0, writeEV ev1, writeEV ev2]
--   apply' f args
-- 
-- call3 fname = call3' (Symbol fname)
-- 
-- callInteractively
--   :: Text
--   -> EmacsM EmacsValue
-- callInteractively fname =
--   call1 "call-interactively" (Symbol fname)
-- 

