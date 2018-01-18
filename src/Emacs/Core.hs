{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.Core
  ( EmacsValue
  , TypedEmacsValue
  , EmacsSymbol, EmacsString, EmacsInteger, EmacsFunction
  , Doc(..), Arity(..)
  , unsafeType, untype
  , EmacsM
  , isEq
  , isNil
  -- , mkNil
  -- , mkList
  -- , mkString
  -- , mkInteger
  -- , intern
  -- mk系、read系足す？？
  -- readEV, writeEV は公開すべきなのか？？？
  -- mkFunの中だけで利用するべきだけでは？？
  , WriteEmacsValue(..), ReadEmacsValue(..)
  , ToEmacsCallable(..)
  , apply'
  , call0', call1', call2', call3', call0, call1, call2, call3
  , mkFun
  , mkFun0, mkFun1, mkFun2
  , mkIOFun0, mkIOFun1
  , Fn0(..), Fn1(..), Fn2(..)
  , IOFn0(..), IOFn1(..), IOFn2(..)
  , Symbol(..)
  , Keyword(..)
  ) where

import Prelude()
import Protolude hiding (Symbol)
import Emacs.Internal hiding (isNil, intern)
import qualified Emacs.Internal as I
import Foreign.Ptr (nullPtr)
import Foreign.StablePtr (castPtrToStablePtr)

-- * EmacsM モナド
--
-- Readerパターンにでも基づこうかと考えていたが、必要ないかな。

newtype EmacsM a = EmacsM (ReaderT EmacsEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadReader EmacsEnv EmacsM

runEmacsM :: EmacsEnv -> EmacsM a -> IO a
runEmacsM env (EmacsM act) = runReaderT act env

liftEM :: (EmacsEnv -> a -> IO b) -> a -> EmacsM b
liftEM f a = do
  env <- ask
  liftIO $ f env a

liftEM2 :: (EmacsEnv -> a -> b -> IO c) -> a -> b -> EmacsM c
liftEM2 f a b = do
  env <- ask
  liftIO $ f env a b

liftEM3 :: (EmacsEnv -> a -> b -> c -> IO d) -> a -> b -> c -> EmacsM d
liftEM3 f a b c = do
  env <- ask
  liftIO $ f env a b c

isEq :: EmacsValue -> EmacsValue -> EmacsM Bool
isEq = liftEM2 I.eq

isNil :: EmacsValue -> EmacsM Bool
isNil = liftEM I.isNil

-- Emacs の値に対する Haskell の型
-- 数値や文字列は素直なんだけど、他
-- Nil は空 [] でいいのかな？
newtype Symbol = Symbol Text

-- Emacs の内部的にはキーワードは : (コロン)から始まるシンボル
-- 値として評価すると自身が返る。
newtype Keyword = Keyword Text

-- * ReadEmacsValue class
--
-- Opaqueな型に変換する場合、型チェックを行なった上で変換すること。

class ReadEmacsValue r where
  readEV :: EmacsValue -> EmacsM r

newtype EmacsValueConversionException = EmacsValueConversionException Text
  deriving (Show)

instance Exception EmacsValueConversionException

instance ReadEmacsValue EmacsValue where
  readEV = pure

-- ** TypeEmacsValue 系の instance

typeCheckWithP :: Text -> EmacsValue -> EmacsM (TypedEmacsValue et)
typeCheckWithP p ev = do
  b <- isNil =<< call1 p ev
  if b
    then throwIO $ EmacsValueConversionException $ "Failed:" <> p
    else pure $ TypedEmacsValue ev

instance ReadEmacsValue (TypedEmacsValue EmacsSymbol) where
  readEV = typeCheckWithP "symbolp"

instance ReadEmacsValue (TypedEmacsValue EmacsString) where
  readEV = typeCheckWithP "stringp"

instance ReadEmacsValue (TypedEmacsValue EmacsInteger) where
  readEV = typeCheckWithP "integerp"

instance ReadEmacsValue (TypedEmacsValue EmacsFunction) where
  readEV = typeCheckWithP "functionp"

-- ** emacsプリミティブ型 の instance

instance ReadEmacsValue Text where
  readEV = liftEM I.extractString . unsafeType

instance ReadEmacsValue Int where
  readEV = liftEM I.extractInteger . unsafeType

instance ReadEmacsValue Symbol where
  readEV ev = Symbol <$> (readEV =<< call1 "symbol-name" ev)

instance ReadEmacsValue () where
  readEV ev =
    ifM (isNil ev)
      (pure ())
      (throwIO $ EmacsValueConversionException "not nil")

-- emacs には bool 型は存在しないことに注意。
-- nil のみが false, 他は真扱い。
instance ReadEmacsValue Bool where
  readEV ev = not <$> isNil ev

-- ** Mabye instance: nil もしくは 値がある場合に使う

instance ReadEmacsValue r => ReadEmacsValue (Maybe r) where
  readEV ev = do
    v <- isNil ev
    if v
      then pure Nothing
      else Just <$> readEV ev

-- * WriteEmacsValue class

class WriteEmacsValue w where
  writeEV :: w -> EmacsM EmacsValue

instance WriteEmacsValue EmacsValue where
  writeEV = pure

instance WriteEmacsValue (TypedEmacsValue et) where
  writeEV = pure . untype

instance WriteEmacsValue Symbol where
  writeEV (Symbol s) = untype <$> liftEM I.intern s

instance WriteEmacsValue Keyword where
  writeEV (Keyword s) = writeEV $ Symbol $ ":" <> s

instance WriteEmacsValue Text where
  writeEV t = untype <$> liftEM I.makeString t

-- emacs には Bool 型はないことに注意
instance WriteEmacsValue Bool where
  writeEV True = mkT
  writeEV False = mkNil

instance WriteEmacsValue () where
  writeEV _ = mkNil

instance WriteEmacsValue a => WriteEmacsValue [a] where
  writeEV evs =
    mkList =<< traverse writeEV evs

instance WriteEmacsValue a => WriteEmacsValue (Maybe a) where
  writeEV Nothing = mkNil
  writeEV (Just a) = writeEV a

-- * funcall: 関数呼び出しの効率化

-- Symbol or a Function
-- ただ シンボルに functoin が設定されているかどうかまでは確認しない。
-- Text も特別に便宜のため対応させる -> OverloadedString と相性が悪いので止め
class WriteEmacsValue c => ToEmacsCallable c where
  toCallableEmacsValue :: c -> EmacsM EmacsValue
  toCallableEmacsValue = writeEV

instance ToEmacsCallable Symbol 
instance ToEmacsCallable (TypedEmacsValue EmacsSymbol)
instance ToEmacsCallable (TypedEmacsValue EmacsFunction) 

-- apply/call
--
-- 最初は返値も ReadEmacsValue を用いて多相的に書いていたのだが、
--
--   * EmacsM () のために Nilの導入
--   * void' が必要に
--   * 自分の足を打ち抜きそうになるので
--
-- 多相的な結果型って何かアンチパターンの匂いがするんだけど、あまり言及見つからないな...

apply'
  :: (ToEmacsCallable f)
  => f
  -> [EmacsValue]
  -> EmacsM EmacsValue
apply' f args = do
  let r = liftEM2 funcall <$> (toCallableEmacsValue f) <*> (pure args)
  readEV =<< join r

call0'
  :: (ToEmacsCallable f)
  => f
  -> EmacsM EmacsValue
call0' f = do
  apply' f []

call0 fname = call0' (Symbol fname)

call1'
  :: (ToEmacsCallable f, WriteEmacsValue a)
  => f
  -> a
  -> EmacsM EmacsValue
call1' f ev0 = do
  args <- sequence [writeEV ev0]
  apply' f args

call1 fname = call1' (Symbol fname)

call2'
  :: (ToEmacsCallable f, WriteEmacsValue a, WriteEmacsValue b)
  => f
  -> a
  -> b
  -> EmacsM EmacsValue
call2' f ev0 ev1 = do
  args <- sequence [writeEV ev0, writeEV ev1]
  apply' f args

call2 fname = call2' (Symbol fname)

call3'
  :: (ToEmacsCallable f, WriteEmacsValue a, WriteEmacsValue b, WriteEmacsValue c)
  => f
  -> a
  -> b
  -> c
  -> EmacsM EmacsValue
call3' f ev0 ev1 ev2 = do
  args <- sequence [writeEV ev0, writeEV ev1, writeEV ev2]
  apply' f args

call3 fname = call3' (Symbol fname)

callInteractively
  :: Text
  -> EmacsM EmacsValue
callInteractively fname =
  call1 "call-interactively" (Symbol fname)

-- * 関数作成

mkFun
  :: Doc
  -> (Arity,Arity)
  -> ([EmacsValue] -> EmacsM EmacsValue)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun doc (Arity minArity,Arity maxArity) f = do
  env <- ask
  liftIO $ I.makeFunction env f' minArity maxArity doc (castPtrToStablePtr nullPtr)
  where
    f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
    f' env _ args = runEmacsM env $ f args

mkFun1'
  :: (ReadEmacsValue a)
  => (r -> EmacsM EmacsValue)
  -> Doc
  -> (a -> r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun1' conv doc f = do
  env <- ask
  liftIO $ I.makeFunction env f' 1 1 doc (castPtrToStablePtr nullPtr)
  where
    -- 引数が一つであることは、arityの設定により Emacs が保証してくれるはず？
    f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
    f' env _ [arg0'] = runEmacsM env $ do
      arg0 <- readEV arg0'
      conv $ f arg0

-- ** 0 と 2 以上の arity分

mkFun0'
  :: (r -> EmacsM EmacsValue)
  -> Doc
  -> r
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun0' conv doc f = do
  env <- ask
  liftIO $ I.makeFunction env f' 0 0 doc (castPtrToStablePtr nullPtr)
  where
    f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
    f' env _ [] = runEmacsM env $ do
      conv $ f

mkFun2'
  :: (ReadEmacsValue a, ReadEmacsValue b)
  => (r -> EmacsM EmacsValue)
  -> Doc
  -> (a -> b -> r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun2' conv doc f = do
  env <- ask
  liftIO $ I.makeFunction env f' 2 2 doc (castPtrToStablePtr nullPtr)
  where
    f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
    f' env _ [arg0', arg1'] = runEmacsM env $ do
      arg0 <- readEV arg0'
      arg1 <- readEV arg1'
      conv $ f arg0 arg1

mkFun3'
  :: (ReadEmacsValue a, ReadEmacsValue b, ReadEmacsValue c)
  => (r -> EmacsM EmacsValue)
  -> Doc
  -> (a -> b -> c -> r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun3' conv doc f = do
  env <- ask
  liftIO $ I.makeFunction env f' 3 3 doc (castPtrToStablePtr nullPtr)
  where
    f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
    f' env _ [arg0', arg1', arg2'] = runEmacsM env $ do
      arg0 <- readEV arg0'
      arg1 <- readEV arg1'
      arg2 <- readEV arg2'
      conv $ f arg0 arg1 arg2

mkFun4'
  :: (ReadEmacsValue a, ReadEmacsValue b, ReadEmacsValue c, ReadEmacsValue d)
  => (r -> EmacsM EmacsValue)
  -> Doc
  -> (a -> b -> c -> d -> r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun4' conv doc f = do
  env <- ask
  liftIO $ I.makeFunction env f' 4 4 doc (castPtrToStablePtr nullPtr)
  where
    f' :: EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue
    f' env _ [arg0', arg1', arg2', arg3'] = runEmacsM env $ do
      arg0 <- readEV arg0'
      arg1 <- readEV arg1'
      arg2 <- readEV arg2'
      arg3 <- readEV arg3'
      conv $ f arg0 arg1 arg2 arg3

-- ** 純粋関数: mkFun*

mkFun0 :: (WriteEmacsValue r) => Doc -> r -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun0 = mkFun0' writeEV
mkFun1 :: (ReadEmacsValue a, WriteEmacsValue r) => Doc -> (a -> r) -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun1 = mkFun1' writeEV
mkFun2 :: (ReadEmacsValue a, ReadEmacsValue b, WriteEmacsValue r) => Doc -> (a -> b -> r) -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun2 = mkFun2' writeEV

-- ** IO関数: mkIOFun*

class EmacsMonad m where liftToEmacsM :: m a -> EmacsM a
instance EmacsMonad IO where liftToEmacsM = liftIO
instance EmacsMonad EmacsM  where liftToEmacsM = identity

mkIOFun0 :: (WriteEmacsValue r, EmacsMonad m) => Doc -> m r -> EmacsM (TypedEmacsValue EmacsFunction)
mkIOFun0 = mkFun0' (\r -> writeEV =<< liftToEmacsM r)
mkIOFun1 :: (ReadEmacsValue a, WriteEmacsValue r, EmacsMonad m) => Doc -> (a -> m r) -> EmacsM (TypedEmacsValue EmacsFunction)
mkIOFun1 = mkFun1' (\r -> writeEV =<< liftToEmacsM r)

-- * 関数型 Fn, IOFn

newtype Fn0 r = Fn0 r
newtype Fn1 a r = Fn1 (a -> r)
newtype Fn2 a b r = Fn2 (a -> b -> r)

newtype IOFn0 m r = IOFn0 (m r)
newtype IOFn1 m a r = IOFn1 (a -> m r)
newtype IOFn2 m a b r = IOFn2 (a -> b -> m r)

instance (EmacsMonad m, ReadEmacsValue a, WriteEmacsValue r)
      => WriteEmacsValue (IOFn1 m a r) where
  writeEV (IOFn1 f) = untype <$> mkIOFun1 (Doc "") f

-- * その他

-- nil 型は存在しない
-- また nil 値はプロパティを持っていないため、opaque な値を用意する必要はない。
-- 単一の値しかないので引数は不要。どうやって取得するだろ？
-- nil という定数が nil を持っている。
-- (symbol-value 'nil) でいけるかな。(eval 'nil) でもいいかも
--
-- TODO: キャッシュするべきだよね(キャッシュする場合は emacs_value を
-- emacs側でGCされないように global_ref を作る必要があるのかな？
mkNil :: EmacsM EmacsValue
mkNil = call1 "symbol-value" (Symbol "nil")

-- Emacs には boolean型は存在しない。
-- nil が false、nil以外が true と見做される。
-- ただ慣習として t シンボルが true の値として良く利用される。
mkT :: EmacsM EmacsValue
mkT = call1 "symbol-value" (Symbol "t")

-- そもそも list という型は emacs側には存在しない。
-- listp という関数があるが、これは cons もしくは nil かどうかを判定している。
mkList :: [EmacsValue] -> EmacsM EmacsValue
mkList evs = apply' (Symbol "list") evs

-- -- `read-from-string` が失敗した場合は例外投げるので cons かどうかの確
-- -- 認は不要。
-- evalString :: Text -> EmacsM EmacsValue
-- evalString t =
--   funcall1 "eval" =<< (car . typeUnsafe =<< funcall1 "read-from-string" t)
--
-- -- emacsModuleInit に渡す関数
-- -- TODO: move to Module.hs
-- defmodule :: Text -> EmacsM a -> EmacsModule
-- defmodule name mod ert = do
--   env <- getEmacsEnvFromRT ert
--   errorHandle env $ do
--     ctx <- initCtx env
--     runEmacsM ctx $ mod >> funcall1 "provide" (Symbol name)
--   return 0
--
