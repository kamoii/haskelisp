{-# LANGUAGE TypeFamilies #-}
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
  ( EmacsFun  -- メソッドはわざと公開いていない
  , FunType(..)
  , emacsFun
  , funWithDoc
  , fun
  , ToEmacsValue(..)
  , UnsafeReadEmacsValue(..)
  , setq
  , getq, getqDefault, modifyq
  , setf, setfWithDoc
  , setc, setcWithDoc
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
-- 利便性でかなり劣る。Overlapping が正しく解決されているのであれば
-- そちらを利用するべきかな？
--
-- 使う側でも TypeApplications と DataKind が必要。


-- Closed type families を利用した Overlapping/Mulitparameter明示指定回避方法。
-- 参照 https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html
--
type family FT a :: FunType where
  FT (IO a) = IOFun
  FT (EmacsM a) = IOFun
  FT (a -> b) = FT b
  FT a = PureFun

type family AR a :: Nat where
  AR (a -> b) = AR b + 1
  AR a = 0

class EmacsFun a where
  toEmacsFun :: a -> [EmacsValue] -> EmacsM  (Either Text EmacsValue)
  arity :: Int

instance (FT a ~ ft, AR a ~ n, KnownNat n, EmacsFun' ft n a) => EmacsFun a where
  toEmacsFun = toEmacsFun' @ft @n
  arity = fromInteger $ natVal (Proxy :: Proxy n)

data FunType = PureFun | IOFun

class EmacsFun' (ft :: FunType) (n :: Nat) a where
  toEmacsFun' :: a -> [EmacsValue] -> EmacsM  (Either Text EmacsValue)

instance ToEmacsValue a => EmacsFun' PureFun 0 a where
  toEmacsFun' a [] = Right <$> toEv a

instance ToEmacsValue a => EmacsFun' IOFun 0 (IO a) where
  toEmacsFun' a [] = do
    v <- liftIO a
    Right <$> toEv v

instance ToEmacsValue a => EmacsFun' IOFun 0 (EmacsM a) where
  toEmacsFun' a [] = do
    v <- a
    Right <$> toEv v

instance (UnsafeReadEmacsValue a, EmacsFun' ft (n-1) b) => EmacsFun' ft n (a -> b) where
  toEmacsFun' f (ev:evs) = do
    a <- unsafeReadEv ev
    toEmacsFun' @ft @(n-1) (f a) evs

-- 外から typeApplication で指定してもらうために forall ft n a が必要。
emacsFun :: forall a. EmacsFun a => a -> [EmacsValue] -> EmacsM EmacsValue
emacsFun a evs = do
  let argNum = arity @a
  when (argNum /= length evs)
    $ throwIO $ ErrorCall "Indifferent arg num."
  toEmacsFun a evs >>= \case
    Left t -> throwIO $ ErrorCall $ toS t
    Right v -> pure v

funWithDoc :: forall a. EmacsFun a => Doc -> a -> EmacsM (TypedEmacsValue EmacsFunction)
funWithDoc doc a = do
  let ar = arity @a
  mkFun doc (Arity ar, Arity ar) (emacsFun a)

fun :: forall a. EmacsFun a => a -> EmacsM (TypedEmacsValue EmacsFunction)
fun = funWithDoc (Doc "")

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

setq
  :: ToEmacsValue ev
  => Text
  -> ev
  -> EmacsM ()
setq name ev =
  void $ setValue name =<< toEv ev

-- If not bounded symbol, exception happens.
getq
  :: UnsafeReadEmacsValue r
  => Text
  -> EmacsM r
getq name =
  unsafeReadEv =<< getValue name

getqDefault
  :: UnsafeReadEmacsValue r
  => Text
  -> EmacsM r
getqDefault name =
  unsafeReadEv =<< getDefaultValue name

-- If not bounded symbol, exception happens.
modifyq
  :: (UnsafeReadEmacsValue a, ToEmacsValue v)
  => Text
  -> (a -> EmacsM v)
  -> EmacsM ()
modifyq name f = do
  v <- f =<< getq name
  setq name =<< toEv v

setfWithDoc
  :: forall a
   . EmacsFun a
  => Text
  -> Doc
  -> a
  -> EmacsM ()
setfWithDoc name doc a =
  setFunction name =<< funWithDoc doc a

setf
  :: forall a
   . EmacsFun a
  => Text
  -> a
  -> EmacsM ()
setf name = setfWithDoc name (Doc "")

setcWithDoc
  :: forall a
   . EmacsFun a
  => Text
  -> Doc
  -> InteractiveForm
  -> a
  -> EmacsM ()
setcWithDoc name doc iform a =
  setCommand name iform =<< funWithDoc doc a

setc
  :: forall a
   . EmacsFun a
  => Text
  -> InteractiveForm
  -> a
  -> EmacsM ()
setc name = setcWithDoc name (Doc "")

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
