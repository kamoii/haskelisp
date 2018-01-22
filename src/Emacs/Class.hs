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
module Emacs.Class where

import Prelude()
import Protolude hiding (Symbol)
import Emacs.Core
import GHC.TypeLits

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

data FunType = FTPure | FTIO

class PureFun (ft :: FunType) (n :: Nat) a where
  pureApply :: a -> [EmacsValue] -> EmacsM  (Either Text EmacsValue)

instance ToEmacsValue a => PureFun FTPure 0 a where
  pureApply a [] = Right <$> toEv a

instance ToEmacsValue a => PureFun FTIO 0 (IO a) where
  pureApply a [] = do
    v <- liftIO a
    Right <$> toEv v

instance ToEmacsValue a => PureFun FTIO 0 (EmacsM a) where
  pureApply a [] = do
    v <- a
    Right <$> toEv v

instance (FromEmacsValue a, PureFun ft (n-1) b) => PureFun ft n (a -> b) where
  pureApply f (ev:evs) = do
    a <- fromEv ev
    pureApply @ft @(n-1) (f a) evs

-- 外から typeApplication で指定してもらうために forall ft n a が必要。  
tofun :: forall ft n a. PureFun ft n a => a -> [EmacsValue] -> EmacsM EmacsValue  
tofun a evs = 
  pureApply @ft @n a evs >>= \case
    Left t -> undefined
    Right v -> pure v

foo :: [EmacsValue] -> EmacsM EmacsValue
foo = tofun @FTPure @1 (identity :: () -> ())  
bar :: [EmacsValue] -> EmacsM EmacsValue
bar = tofun @FTIO @1 (pure :: () -> IO ())  

class ToEmacsValue a where toEv :: a -> EmacsM EmacsValue
class FromEmacsValue a where fromEv :: EmacsValue -> EmacsM a

instance ToEmacsValue () where toEv = (const mkNil)                             
instance FromEmacsValue () where fromEv = const $ pure ()

-- * ReadEmacsValue class
--
-- Opaqueな型に変換する場合、型チェックを行なった上で変換すること。

-- class ReadEmacsValue r where
--   readEV :: EmacsValue -> EmacsM r
-- 
-- newtype EmacsValueConversionException = EmacsValueConversionException Text
--   deriving (Show)
-- 
-- instance Exception EmacsValueConversionException
-- 
-- instance ReadEmacsValue EmacsValue where
--   readEV = pure
-- 
-- -- ** TypeEmacsValue 系の instance
-- 
-- typeCheckWithP :: Text -> EmacsValue -> EmacsM (TypedEmacsValue et)
-- typeCheckWithP p ev = do
--   b <- isNil =<< call1 p ev
--   if b
--     then throwIO $ EmacsValueConversionException $ "Failed:" <> p
--     else pure $ TypedEmacsValue ev
-- 
-- instance ReadEmacsValue (TypedEmacsValue EmacsSymbol) where
--   readEV = typeCheckWithP "symbolp"
-- 
-- instance ReadEmacsValue (TypedEmacsValue EmacsString) where
--   readEV = typeCheckWithP "stringp"
-- 
-- instance ReadEmacsValue (TypedEmacsValue EmacsInteger) where
--   readEV = typeCheckWithP "integerp"
-- 
-- instance ReadEmacsValue (TypedEmacsValue EmacsFunction) where
--   readEV = typeCheckWithP "functionp"
-- 
-- -- ** emacsプリミティブ型 の instance
-- 
-- instance ReadEmacsValue Text where
--   readEV = liftEM I.extractString . unsafeType
-- 
-- instance ReadEmacsValue Int where
--   readEV = liftEM I.extractInteger . unsafeType
-- 
-- instance ReadEmacsValue Symbol where
--   readEV ev = Symbol <$> (readEV =<< call1 "symbol-name" ev)
-- 
-- instance ReadEmacsValue () where
--   readEV ev =
--     ifM (isNil ev)
--       (pure ())
--       (throwIO $ EmacsValueConversionException "not nil")
-- 
-- -- emacs には bool 型は存在しないことに注意。
-- -- nil のみが false, 他は真扱い。
-- instance ReadEmacsValue Bool where
--   readEV ev = not <$> isNil ev
-- 
-- -- ** Mabye instance: nil もしくは 値がある場合に使う
-- 
-- instance ReadEmacsValue r => ReadEmacsValue (Maybe r) where
--   readEV ev = do
--     v <- isNil ev
--     if v
--       then pure Nothing
--       else Just <$> readEV ev
-- 
-- -- * WriteEmacsValue class
-- 
-- class WriteEmacsValue w where
--   writeEV :: w -> EmacsM EmacsValue
-- 
-- instance WriteEmacsValue EmacsValue where
--   writeEV = pure
-- 
-- instance WriteEmacsValue (TypedEmacsValue et) where
--   writeEV = pure . untype
-- 
-- instance WriteEmacsValue Symbol where
--   writeEV (Symbol s) = untype <$> liftEM I.intern s
-- 
-- instance WriteEmacsValue Keyword where
--   writeEV (Keyword s) = writeEV $ Symbol $ ":" <> s
-- 
-- instance WriteEmacsValue Text where
--   writeEV t = untype <$> liftEM I.makeString t
-- 
-- -- emacs には Bool 型はないことに注意
-- instance WriteEmacsValue Bool where
--   writeEV True = mkT
--   writeEV False = mkNil
-- 
-- instance WriteEmacsValue () where
--   writeEV _ = mkNil
-- 
-- instance WriteEmacsValue a => WriteEmacsValue [a] where
--   writeEV evs =
--     mkList =<< traverse writeEV evs
-- 
-- instance WriteEmacsValue a => WriteEmacsValue (Maybe a) where
--   writeEV Nothing = mkNil
--   writeEV (Just a) = writeEV a
-- 
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
-- -- * 関数作成
-- 
-- mkFun1'
--   :: (ReadEmacsValue a)
--   => (r -> EmacsM EmacsValue)
--   -> Doc
--   -> (a -> r)
--   -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun1' conv doc f = do
--   env <- ask
--   liftIO $ I.makeFunction env f' 1 1 doc (castPtrToStablePtr nullPtr)
--   where
--     -- 引数が一つであることは、arityの設定により Emacs が保証してくれるはず？
--     f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
--     f' env _ [arg0'] = runEmacsM env $ do
--       arg0 <- readEV arg0'
--       conv $ f arg0
-- 
-- -- ** 0 と 2 以上の arity分
-- 
-- mkFun0'
--   :: (r -> EmacsM EmacsValue)
--   -> Doc
--   -> r
--   -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun0' conv doc f = do
--   env <- ask
--   liftIO $ I.makeFunction env f' 0 0 doc (castPtrToStablePtr nullPtr)
--   where
--     f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
--     f' env _ [] = runEmacsM env $ do
--       conv $ f
-- 
-- mkFun2'
--   :: (ReadEmacsValue a, ReadEmacsValue b)
--   => (r -> EmacsM EmacsValue)
--   -> Doc
--   -> (a -> b -> r)
--   -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun2' conv doc f = do
--   env <- ask
--   liftIO $ I.makeFunction env f' 2 2 doc (castPtrToStablePtr nullPtr)
--   where
--     f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
--     f' env _ [arg0', arg1'] = runEmacsM env $ do
--       arg0 <- readEV arg0'
--       arg1 <- readEV arg1'
--       conv $ f arg0 arg1
-- 
-- mkFun3'
--   :: (ReadEmacsValue a, ReadEmacsValue b, ReadEmacsValue c)
--   => (r -> EmacsM EmacsValue)
--   -> Doc
--   -> (a -> b -> c -> r)
--   -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun3' conv doc f = do
--   env <- ask
--   liftIO $ I.makeFunction env f' 3 3 doc (castPtrToStablePtr nullPtr)
--   where
--     f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
--     f' env _ [arg0', arg1', arg2'] = runEmacsM env $ do
--       arg0 <- readEV arg0'
--       arg1 <- readEV arg1'
--       arg2 <- readEV arg2'
--       conv $ f arg0 arg1 arg2
-- 
-- mkFun4'
--   :: (ReadEmacsValue a, ReadEmacsValue b, ReadEmacsValue c, ReadEmacsValue d)
--   => (r -> EmacsM EmacsValue)
--   -> Doc
--   -> (a -> b -> c -> d -> r)
--   -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun4' conv doc f = do
--   env <- ask
--   liftIO $ I.makeFunction env f' 4 4 doc (castPtrToStablePtr nullPtr)
--   where
--     f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
--     f' env _ [arg0', arg1', arg2', arg3'] = runEmacsM env $ do
--       arg0 <- readEV arg0'
--       arg1 <- readEV arg1'
--       arg2 <- readEV arg2'
--       arg3 <- readEV arg3'
--       conv $ f arg0 arg1 arg2 arg3
-- 
-- -- ** 純粋関数: mkFun*
-- 
-- mkFun0 :: (WriteEmacsValue r) => Doc -> r -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun0 = mkFun0' writeEV
-- mkFun1 :: (ReadEmacsValue a, WriteEmacsValue r) => Doc -> (a -> r) -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun1 = mkFun1' writeEV
-- mkFun2 :: (ReadEmacsValue a, ReadEmacsValue b, WriteEmacsValue r) => Doc -> (a -> b -> r) -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkFun2 = mkFun2' writeEV
-- 
-- -- ** IO関数: mkIOFun*
-- 
-- class EmacsMonad m where liftToEmacsM :: m a -> EmacsM a
-- instance EmacsMonad IO where liftToEmacsM = liftIO
-- instance EmacsMonad EmacsM  where liftToEmacsM = identity
-- 
-- mkIOFun0 :: (WriteEmacsValue r, EmacsMonad m) => Doc -> m r -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkIOFun0 = mkFun0' (\r -> writeEV =<< liftToEmacsM r)
-- mkIOFun1 :: (ReadEmacsValue a, WriteEmacsValue r, EmacsMonad m) => Doc -> (a -> m r) -> EmacsM (TypedEmacsValue EmacsFunction)
-- mkIOFun1 = mkFun1' (\r -> writeEV =<< liftToEmacsM r)
-- 
-- -- * 関数型 Fn, IOFn
-- 
-- newtype Fn0 r = Fn0 r
-- newtype Fn1 a r = Fn1 (a -> r)
-- newtype Fn2 a b r = Fn2 (a -> b -> r)
-- 
-- newtype IOFn0 m r = IOFn0 (m r)
-- newtype IOFn1 m a r = IOFn1 (a -> m r)
-- newtype IOFn2 m a b r = IOFn2 (a -> b -> m r)
-- 
-- instance (EmacsMonad m, ReadEmacsValue a, WriteEmacsValue r)
--       => WriteEmacsValue (IOFn1 m a r) where
--   writeEV (IOFn1 f) = untype <$> mkIOFun1 (Doc "") f
