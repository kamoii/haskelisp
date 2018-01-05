{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.Core
  ( module Emacs.Type
  , initState
  , initCtx
  , getEnv
  , runEmacsM
  , getEmacsEnvFromRT
  -- , typeOf
  -- , isTypeOf
  , eq
  , isNotNil
  , isNil
  , mkFunction
  , mkInteger
  , mkString
  , intern
  , mkCons
  , mkList
  , mkNil
  , mkT
  , extractInteger
  , extractString
  , funcall
  , funcall1, funcall2, funcall3
  , callInteractively
  , errorHandle
  , defmodule
  , CheckTypeEmacsValue(..)
  , ToTypedEmacsValue(..)
  , ToEmacsValue(..)
  , ToEmacsSymbol
  , ToEmacsFunction
  , FromEmacsValue(..)
  , mkFunctionFromFunctionable
  , Functionable(..)
  , car
  , cdr
  , evalString
  , provide
  , message
  , print
  , typeCheckWithP
  ) where

import Prelude(error)
import Protolude hiding (mkInteger,print,Symbol)
import Control.Exception (displayException)
import Data.IORef
import Emacs.Type
import qualified Data.List as List
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import GHC.Ptr
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding.UTF8 (utf8)

initState :: MonadIO m => m PState
initState = do
  mapRef <- liftIO $ newIORef mempty
  return $ PState mapRef

initCtx :: MonadIO m => EmacsEnv -> m Ctx
initCtx env = do
  pstate <- initState
  pstatep <- liftIO $ newStablePtr pstate
  return $ Ctx pstatep pstate env

getPStateStablePtr :: EmacsM (StablePtr PState)
getPStateStablePtr =
  pstateStablePtr <$> ask

getPState :: EmacsM PState
getPState =
  pstate <$> ask

getEnv :: EmacsM EmacsEnv
getEnv =
  emacsEnv <$> ask

-- Logging here is not a good idea. When passing high order function,
-- which could be invoked manytimes, its get quite slow.
runEmacsM :: MonadIO m => Ctx -> EmacsM a -> m a
runEmacsM ctx action =
  liftIO $ runReaderT action ctx

foreign import ccall _get_emacs_env_from_rt
  :: Ptr ()
  -> IO EmacsEnv

getEmacsEnvFromRT :: Ptr () -> IO EmacsEnv
getEmacsEnvFromRT =
  _get_emacs_env_from_rt

foreign import ccall _type_of
  :: EmacsEnv
  -> EmacsValue
  -> IO EmacsValue

-- 現状利用不可。
-- 問題は EmacsType と一対一で対応しない。isTypeOf は使えるかもしれないが、あまり意味はないかな...

-- typeOf :: EmacsValue -> EmacsM EmacsType
-- typeOf ev = do
--   env <- getEnv
--   typeP <- checkExitStatus $ _type_of env ev
--   types <- forM emacsTypes $ \t -> do
--              q <- intern (emacsTypeSymbolName t)
--              b <- eq q typeP
--              return (b, t)
--   case List.find fst types of
--     Just (_, t) -> return t
--     Nothing     -> error "no type"

-- isTypeOf :: (et :: EmacsType) -> EmacsValue -> EmacsM Bool
-- isTypeOf ty ev = do
--   t <- typeOf ev
--   return $ t == ty

-- 引数が integer じゃない場合多分 signal が投げられる
foreign import ccall _extract_integer
  :: EmacsEnv
  -> EmacsValue
  -> IO CIntMax

extractInteger
  :: Num b
  => TypedEmacsValue EmacsInteger
  -> EmacsM b
extractInteger (TypedEmacsValue ev) = do
  env <- getEnv
  i <- checkExitStatus $ _extract_integer env ev
  pure $ fromIntegral i

-- emacs-module.c 参照
--
--  * Can throw signals(その場合 false が返る)
--  * もし Buffer が null の場合、Length に文字列のutf8で格納する際の
--    必要な長さが設定され、1 を返す
--  * もし Buffer が non-null かつ、Length がutf8を格納するのに足りな
--    い場合、Length に必要な長さが設定され args_out_of_rangeエラーが
--    投げられる。
--  * Bufferが non-null かつ、Length が十分な長さを持っている場合、
--    Buffer に utf8文字列(+最後はnull文字)が格納され、Length には長さ
--    (最後のnull文字を含めたもの)が設定され 1 を返す。
--
foreign import ccall _copy_string_contents
  :: EmacsEnv
  -> EmacsValue
  -> CString         -- Buffer
  -> Ptr CPtrdiff    -- Length
  -> IO CInt

extractString
  :: TypedEmacsValue EmacsString
  -> EmacsM Text
extractString (TypedEmacsValue ev) = do
  env <- getEnv
  checkExitStatus $ alloca $ \length' -> do
    result <- _copy_string_contents env ev nullPtr length'
    if result == 1
      then do
        length <- fromIntegral <$> peek length'
        allocaBytes length $ \buffer -> do
          result' <- _copy_string_contents env ev buffer length'
          if result == 1
            then toS <$> GHC.peekCString utf8 buffer
            else pure ""
      else pure ""

-- eq は bool 返すのだが、haskell では CBool は用意していないので int
-- にして返している。module_eq は珍しく MODULE_FUNCTION_BEGIN を使って
-- いない。
foreign import ccall _eq
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO CInt

eq
  :: EmacsValue
  -> EmacsValue
  -> EmacsM Bool
eq ev0 ev1 = do
  env <- getEnv
  r <- liftIO $ _eq env ev0 ev1
  pure $ r == 1

foreign import ccall _is_not_nil
  :: EmacsEnv
  -> EmacsValue
  -> IO CInt

-- isNil, isNotNil に関しては TypedEmacsValue に対して使わないので
-- EmacsValue を引数に取るので問題ないかな。
isNotNil :: EmacsValue -> EmacsM Bool
isNotNil ev = do
  env <- getEnv
  r <- liftIO $ _is_not_nil env ev
  pure $ r == 1

isNil :: EmacsValue -> EmacsM Bool
isNil = (fmap . fmap) not isNotNil

-- TODO: arity と doc は Arity と Doc 型にするべきかな。
foreign import ccall _make_function
  :: EmacsEnv
  -> CPtrdiff
  -> CPtrdiff
  -> FunPtr EFunctionStub
  -> CString
  -> StablePtr a
  -> IO EmacsValue

-- TODO: ??? これ StablePtr の効果も兼ねている？
foreign import ccall "wrapper" wrapEFunctionStub
  :: EFunctionStub
  -> IO (FunPtr EFunctionStub)

mkFunction
  :: ([EmacsValue] -> EmacsM EmacsValue)
  -> Int
  -> Int
  -> Text
  -> EmacsM EmacsFunction
mkFunction f minArity' maxArity' doc' = do
  let minArity = fromIntegral minArity' :: CPtrdiff
      maxArity = fromIntegral maxArity' :: CPtrdiff
  datap <- getPStateStablePtr
  stubp <- liftIO (wrapEFunctionStub stub)
  env <- getEnv
  checkExitStatus . withCString (toS doc') $ \doc ->
    TypedEmacsValue <$> _make_function env minArity maxArity stubp doc datap
  where
    stub :: EFunctionStub
    stub env nargs args pstatep = errorHandle env $ do
      pstate <- deRefStablePtr pstatep
      es <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
      runEmacsM (Ctx pstatep pstate env) (f es)

-- Haskell で投げられた例外の対応
--
-- Emacs -> Haskell から呼ばれるところに設置する必要がある。例外が補足
-- できないと恐らく emacs がクラッシュする。非同期例外については考える
-- 必要はない。
--
-- 二つの場合を対処する必要がある(多段)
--
--  1. Haskell 側で例外が発生した
--  2. Haskell から呼び出した emacs 関数の中で signal(or throw)された
--
-- 2. の場合、emacsから返ってきた時に non local exit かどうか確認し、
-- もしそうであれば haskellの例外を投げる。そして haskell -> emacsに戻
-- る場所で haskellの例外は補足する。その場合、non-local-exit は既に設
-- 定されているので、
--
-- _non_local_exit_signal で haskellエラーであることを設定する。ただし
-- これが簡単にはいかず、
--
--   * IO モナドの中で実現する必要がある
--   * emacs関数を呼び出す際に例外が発生しうるものを呼び出せない
--
-- catch する順番重要
errorHandle :: EmacsEnv -> IO EmacsValue -> IO EmacsValue
errorHandle env action =
  action `catch` emacsExceptionHandler
         `catch` haskellExceptionHandler
  where
    -- handler の中で例外が発生した場合は諦め？
    -- ほんとは ctx はいらなくて、env だけで 対応したいところ
    -- とりあえずの対応
    --
    -- TODO: ハンドラ中に EmacsException が投げられたときは無視しない
    -- といけな？
    haskellExceptionHandler :: SomeException -> IO EmacsValue
    haskellExceptionHandler e = do
      ctx <- initCtx env
      runEmacsM ctx $ do
        funcallExit <- nonLocalExitCheck
        -- TODO: これが不味い。既に funcall-exit が signal/throw に設
        -- 定されている可能性があるため、mkNil で更に EmacsException
        -- 例外が飛んでしまう。
        nil <- untype <$> mkNil
        when (funcallExit == EmacsFuncallExitReturn) $ do
          mes <- mkString (toS $ displayException e)
          (TypedEmacsValue arg) <- mkList [untype mes]
          sym <- intern "haskell-error"
          nonLocalExitSignal sym arg -- これ以降 emacs関数を呼んでは駄目
        return nil

    emacsExceptionHandler :: EmacsException -> IO EmacsValue
    emacsExceptionHandler e@(EmacsException funcallExit a0 a1) = do
      let setter = case funcallExit of
                     EmacsFuncallExitSignal -> _non_local_exit_signal
                     EmacsFuncallExitThrow -> _non_local_exit_throw
      setter env a0 a1
      return a0

-- emacsモジュール関数の呼び出し後に signal/throwされていないかチェッ
-- クする。されている場合は EmacsException を投げる。
--
-- emacs-module.c の module_* 関数で 先頭にMODULE_FUNCTION_BEGIN が書
-- かれているものが実行の後にチェックが必要。
--
-- TODO: 理想的には チェック必要な import ccal は IONeedCheck a みたい
-- な型を返すようにして、checkExitStatus しないと IO(や EmacsM)に直せ
-- ないようにするのがいいのかな？ただちょっと面倒。
checkExitStatus :: IO a -> EmacsM a
checkExitStatus action = do
  v <- liftIO action
  funcallExit <- nonLocalExitCheck
  when (funcallExit /= EmacsFuncallExitReturn) $ do
    (_,a0,a1) <- nonLocalExitGet
    nonLocalExitClear
    liftIO . throwIO $ EmacsException funcallExit a0 a1
  return v

--   emacs_value (*make_integer) (emacs_env *env, intmax_t value);
foreign import ccall _make_integer
  :: EmacsEnv
  -> CIntMax
  -> IO EmacsValue

mkInteger :: Integral n => n -> EmacsM (TypedEmacsValue EmacsInteger)
mkInteger i' = do
  let i = fromIntegral i' :: CIntMax
  env <- getEnv
  checkExitStatus $ TypedEmacsValue <$> _make_integer env i

-- Create emacs symbol
foreign import ccall _make_string
  :: EmacsEnv
  -> CString
  -> CPtrdiff
  -> IO EmacsValue

mkString :: Text -> EmacsM (TypedEmacsValue EmacsString)
mkString str = do
  env <- getEnv
  checkExitStatus . withCStringLen (toS str) $ \(cstr,len) ->
    TypedEmacsValue <$> _make_string env cstr (fromIntegral len)

-- Symbol
-- https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html
--
-- intern という名前にしたのは不味い気がしてきた。elispには intern
-- と make-symbol があり意味が違う。intern はシンボルを obarray に登録
-- する(既に登録されていればそれを返す)。make-symbol は全く新しいシン
-- ボルを作成し、obarray には登録しない。
--
-- :foo のようなのは keyword symbol と呼ばれており、自分自身に評価され
-- る。実態としてはただ単に : で前置されたシンボルである。

foreign import ccall _intern
  :: EmacsEnv
  -> CString
  -> IO EmacsValue

-- TODO: キャッシュするのは不味い気がしてきた。滅多にないとは思うけど、
-- unintern された場合にの動きが問題となる。
intern :: Text -> EmacsM (TypedEmacsValue EmacsSymbol)
intern str = do
  s' <- lookupCache
  TypedEmacsValue <$> case s' of
    Just gev ->
      return (castGlobalToEmacsValue gev)
    Nothing ->
      storeToCache =<< create
  where
    lookupCache = do
      mapRef <- symbolMap <$> getPState
      map <- liftIO $ readIORef mapRef
      return $ Map.lookup str map

    -- TODO: 現在は全部入れているけど、これはまずい
    storeToCache ev = do
      mapRef <- symbolMap <$> getPState
      gev <- mkGlobalRef ev
      liftIO $ modifyIORef mapRef (Map.insert str gev)
      return (castGlobalToEmacsValue gev)

    create = do
      env <- getEnv
      checkExitStatus . withCString (toS str) $ \cstr -> _intern env cstr


foreign import ccall _make_global_ref
  :: EmacsEnv
  -> EmacsValue
  -> IO GlobalEmacsValue

mkGlobalRef :: EmacsValue -> EmacsM GlobalEmacsValue
mkGlobalRef ev = do
  env <- getEnv
  checkExitStatus $ _make_global_ref env ev

-- 例外ハンドリング

foreign import ccall _non_local_exit_check
 :: EmacsEnv
 -> IO CInt

nonLocalExitCheck :: EmacsM EmacsFuncallExit
nonLocalExitCheck = do
  env <- getEnv
  toEnum . fromIntegral <$> liftIO (_non_local_exit_check env)

foreign import ccall _non_local_exit_signal
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitSignal
  :: TypedEmacsValue EmacsSymbol
  -> EmacsValue
  -> EmacsM ()
nonLocalExitSignal (TypedEmacsValue sym) val = do
  env <- getEnv
  liftIO $ _non_local_exit_signal env sym val

foreign import ccall _non_local_exit_throw
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitThrow :: EmacsValue -> EmacsValue -> EmacsM ()
nonLocalExitThrow sym val = do
  env <- getEnv
  liftIO $ _non_local_exit_throw env sym val

foreign import ccall _non_local_exit_clear
  :: EmacsEnv
  -> IO ()

nonLocalExitClear :: EmacsM ()
nonLocalExitClear = do
  env <- getEnv
  liftIO $ _non_local_exit_clear env

-- 第二引数、第三引数に書き込まれることに注意。
foreign import ccall _non_local_exit_get
  :: EmacsEnv
  -> Ptr EmacsValue
  -> Ptr EmacsValue
  -> IO CInt

nonLocalExitGet :: EmacsM (EmacsFuncallExit,EmacsValue,EmacsValue)
nonLocalExitGet = do
  env <- getEnv
  liftIO $ do
    a0' <- malloc
    a1' <- malloc
    fe  <- _non_local_exit_get env a0' a1'
    a0  <- peek a0'
    a1  <- peek a1'
    free a0'
    free a1'
    return (toEnum (fromIntegral fe), a0, a1)

foreign import ccall _funcall
  :: EmacsEnv
  -> EmacsValue
  -> CPtrdiff
  -> Ptr EmacsValue
  -> IO EmacsValue

funcall
  :: EmacsValue
  -> [EmacsValue]
  -> EmacsM EmacsValue
funcall func args = do
  env <- getEnv
  checkExitStatus . withArray args $ \carr ->
    _funcall env func argsLen carr
  where
    argsLen = fromIntegral (length args) :: CPtrdiff




