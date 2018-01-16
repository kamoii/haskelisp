{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Emacs.Internal.FFI
  ( emacsModule
  -- , typeOf
  -- , isTypeOf
  , eq
  , isNotNil
  , isNil
  , makeFunction
  , makeInteger
  , makeString
  , intern
  , extractInteger
  , extractString
  , funcall
  ) where

import Prelude()
import Protolude
import Control.Exception (displayException)
import Data.IORef
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import GHC.Ptr
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding.UTF8 (utf8)
import Emacs.Internal.Types

-- | FFI

foreign import ccall _get_emacs_env_from_rt
  :: EmacsRuntime
  -> IO EmacsEnv

getEmacsEnvFromRT :: EmacsRuntime -> IO EmacsEnv
getEmacsEnvFromRT =
  _get_emacs_env_from_rt

foreign import ccall _type_of
  :: EmacsEnv
  -> EmacsValue
  -> IO EmacsValue

emacsModule :: (EmacsEnv -> IO ()) -> EmacsModule
emacsModule act er = do
  env <- getEmacsEnvFromRT er
  errorHandle env (act env) >>= \case
    Just _ -> pure 0
    Nothing -> pure 1

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

-- 引数が integer じゃない場合多分 signal が投げられる
foreign import ccall _extract_integer
  :: EmacsEnv
  -> EmacsValue
  -> IO CIntMax

extractInteger
  :: Num b
  => EmacsEnv
  -> TypedEmacsValue EmacsInteger
  -> IO b
extractInteger env (TypedEmacsValue ev) = do
  i <- checkExitStatus env $ _extract_integer env ev
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
  :: EmacsEnv
  -> TypedEmacsValue EmacsString
  -> IO Text
extractString env (TypedEmacsValue ev) = do
  checkExitStatus env $ alloca $ \length' -> do
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
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO Bool
eq env ev0 ev1 = do
  r <- _eq env ev0 ev1
  pure $ r == 1

foreign import ccall _is_not_nil
  :: EmacsEnv
  -> EmacsValue
  -> IO CInt

-- isNil, isNotNil に関しては TypedEmacsValue に対して使わないので
-- EmacsValue を引数に取るので問題ないかな。
isNotNil
  :: EmacsEnv
  -> EmacsValue
  -> IO Bool
isNotNil env ev = do
  r <- _is_not_nil env ev
  pure $ r == 1

isNil :: EmacsEnv -> EmacsValue -> IO Bool
isNil env = (fmap . fmap) not (isNotNil env)

-- TODO: arity と doc は Arity と Doc 型にするべきかな。
foreign import ccall _make_function
  :: EmacsEnv
  -> CPtrdiff
  -> CPtrdiff
  -> FunPtr (EFunctionStub a)
  -> CString
  -> StablePtr a
  -> IO EmacsValue

-- https://wiki.haskell.org/GHC/Using_the_FFI#Are_FunPtr.27s_stable.3F
-- FunPtr は Stable になる
foreign import ccall "wrapper" wrapEFunctionStub
  :: EFunctionStub a
  -> IO (FunPtr (EFunctionStub a))

makeFunction
  :: forall a
   . EmacsEnv
  -> (EmacsEnv -> StablePtr a -> [EmacsValue] -> IO EmacsValue)
  -> Int
  -> Int
  -> Doc
  -> StablePtr a
  -> IO (TypedEmacsValue EmacsFunction)
makeFunction env func minArity' maxArity' (Doc doc') datap = do
  let minArity = fromIntegral minArity' :: CPtrdiff
      maxArity = fromIntegral maxArity' :: CPtrdiff
  stubp <- wrapEFunctionStub stub
  checkExitStatus env . withCString (toS doc') $ \doc ->
    TypedEmacsValue <$> _make_function env minArity maxArity stubp doc datap
  where
    -- Emacs サイドから Haskell サイドが呼ばれる箇所
    -- stub はクロージャにすると不味い? 例えば EmacsValue -> EFunctionStub とか
    -- いや大丈夫なはず。現に func は外側にあるし(本当か？？)
    -- GCが関係しているはず。
    --
    -- emacs-module.c の funcall_module 関数が 作った関数を呼び出すものだが、
    -- throw/signal されている場合は 返値の値は利用されていないようだ。
    -- そのため例外が発生した場合、返す EmacsValue はヌルポでも良いはず。(たぶん)
    stub :: EFunctionStub a
    stub env nargs args datap = do
      v <- errorHandle env $ do
        evArgs <- fmap EmacsValue <$> peekArray (fromIntegral nargs) args
        func env datap evArgs
      case v of
        Just ev -> pure ev
        Nothing -> pure $ EmacsValue nullPtr

-- Haskell で投げられた例外の対応
--
-- Emacs -> Haskell から呼ばれるところに設置する必要がある。つまり以下
-- の二つの箇所。
--
--  1. モジュールのエントリポイント(load,requireされたときに呼ばれる箇所)
--  2. makeFunction で、haskellで作成したemacs関数が、emacs側で呼ばれたとき
--
-- Haskell に処理が移ったらなべく早く errorHandle の中で実行する必要がある。
-- Haskell で発生した例外が補足できないと恐らく emacs がクラッシュする。
-- (非同期例外については考える必要はない)。
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
errorHandle
  :: EmacsEnv
  -> IO a
  -> IO (Maybe a)
errorHandle env action =
  (Just <$> action)
    `catch` (\e -> emacsExceptionHandler e *> pure Nothing)
    `catch` (\e -> haskellExceptionHandler e *> pure Nothing)
  where
    -- Handler の中で例外が発生した場合は諦め？
    -- TODO: ハンドラ中に EmacsException が投げられたときは無視しない
    -- といけな？
    --
    -- EmacsValue
    haskellExceptionHandler :: SomeException -> IO ()
    haskellExceptionHandler e =
       nonLocalExitGet env >>= \case
         Just _ ->
           pure ()
         Nothing -> do
           message <- makeString env (toS $ displayException e)
           listP <- intern env "list"
           arg <- funcall env (untype listP) [untype message]
           sym <- intern env "haskell-error"
           nonLocalExitSignal env sym arg -- これ以降 emacs関数を呼んでは駄目
           pure ()

    emacsExceptionHandler :: EmacsException -> IO ()
    emacsExceptionHandler e@(EmacsException funcallExit a0 a1) = do
      let setter = case funcallExit of
                     EmacsFuncallExitSignal -> _non_local_exit_signal
                     EmacsFuncallExitThrow -> _non_local_exit_throw
      setter env a0 a1
      pure ()

-- emacsモジュール関数の呼び出し後に signal/throwされていないかチェッ
-- クする。されている場合はクリアして EmacsException を投げる。
--
-- emacs-module.c の module_* 関数で 先頭にMODULE_FUNCTION_BEGIN が書
-- かれているものが実行の後にチェックが必要。
--
-- TODO: 理想的には チェック必要な import ccal は IONeedCheck a みたい
-- な型を返すようにして、checkExitStatus しないと IO(や EmacsM)に直せ
-- ないようにするのがいいのかな？ただちょっと面倒。
-- そこまでする必要はないか。
--
--   * checkExistStatus はこのモジュール外には公開されない
--   * このモジュールが正しく checkExistStatus が呼ばれることの責任を持つ
--   * IONeedCheckの型にし忘れたら結局同じかな...
--   * むしろ ffi関数の命名規則で？
--
checkExitStatus :: EmacsEnv -> IO a -> IO a
checkExitStatus env ccall = do
  v <- ccall
  nonLocalExitGet env >>= \case
    Nothing ->
      pure v
    Just (funcallExit,a0,a1) -> do
      nonLocalExitClear env
      throwIO $ EmacsException funcallExit a0 a1

--   emacs_value (*make_integer) (emacs_env *env, intmax_t value);
foreign import ccall _make_integer
  :: EmacsEnv
  -> CIntMax
  -> IO EmacsValue

makeInteger
  :: Integral n
  => EmacsEnv
  -> n
  -> IO (TypedEmacsValue EmacsInteger)
makeInteger env i' = do
  let i = fromIntegral i' :: CIntMax
  checkExitStatus env $ TypedEmacsValue <$> _make_integer env i

-- Create emacs symbol
foreign import ccall _make_string
  :: EmacsEnv
  -> CString
  -> CPtrdiff
  -> IO EmacsValue

makeString
  :: EmacsEnv
  -> Text
  -> IO (TypedEmacsValue EmacsString)
makeString env str = do
  checkExitStatus env . withCStringLen (toS str) $ \(cstr,len) ->
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

intern
  :: EmacsEnv
  -> Text
  -> IO (TypedEmacsValue EmacsSymbol)
intern env str =
  map unsafeType
    . checkExitStatus env
    . withCString (toS str) $ \cstr -> _intern env cstr

foreign import ccall _make_global_ref
  :: EmacsEnv
  -> EmacsValue
  -> IO GlobalEmacsValue

makeGlobalRef
  :: EmacsEnv
  -> EmacsValue
  -> IO GlobalEmacsValue
makeGlobalRef env ev =
  checkExitStatus env $ _make_global_ref env ev

-- 例外ハンドリング

foreign import ccall _non_local_exit_check
 :: EmacsEnv
 -> IO CInt

nonLocalExitCheck :: EmacsEnv -> IO EmacsFuncallExit
nonLocalExitCheck env =
  toEnum . fromIntegral <$> liftIO (_non_local_exit_check env)

foreign import ccall _non_local_exit_signal
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitSignal
  :: EmacsEnv
  -> TypedEmacsValue EmacsSymbol
  -> EmacsValue
  -> IO ()
nonLocalExitSignal env (TypedEmacsValue sym) val =
  _non_local_exit_signal env sym val

foreign import ccall _non_local_exit_throw
  :: EmacsEnv
  -> EmacsValue
  -> EmacsValue
  -> IO ()

nonLocalExitThrow :: EmacsEnv -> EmacsValue -> EmacsValue -> IO ()
nonLocalExitThrow env sym val =
  _non_local_exit_throw env sym val

foreign import ccall _non_local_exit_clear
  :: EmacsEnv
  -> IO ()

nonLocalExitClear :: EmacsEnv -> IO ()
nonLocalExitClear env =
  _non_local_exit_clear env

-- nonLocalExitGet
-- nonLocalExitCheck と何が違うのは、ExitReturn じゃないときに、例外データを
-- 第二引数、第三引数に書き込んでくれるということ。
-- もしExitReturnの場合、第二引数及び第三引数には何も書かれていない
-- 何が？？？
foreign import ccall _non_local_exit_get
  :: EmacsEnv
  -> Ptr EmacsValue
  -> Ptr EmacsValue
  -> IO CInt

-- EmacsFuncallExitReturn の場合は Nothingを返す
-- EmacsFuncallExit が ExitReturn のとき、peek しては駄目。
nonLocalExitGet :: EmacsEnv -> IO (Maybe (EmacsFuncallExit,EmacsValue,EmacsValue))
nonLocalExitGet env = do
  a0' <- malloc
  a1' <- malloc
  fe' <- _non_local_exit_get env a0' a1'
  let fe = toEnum $ fromIntegral fe'
  re <- if fe == EmacsFuncallExitReturn
    then pure Nothing
    else do
      a0 <- peek a0'
      a1 <- peek a1'
      pure $ Just (fe, a0, a1)
  free a0'
  free a1'
  pure re

-- | Function call

foreign import ccall _funcall
  :: EmacsEnv
  -> EmacsValue
  -> CPtrdiff
  -> Ptr EmacsValue
  -> IO EmacsValue

funcall
  :: EmacsEnv
  -> EmacsValue
  -> [EmacsValue]
  -> IO EmacsValue
funcall env func args = do
  checkExitStatus env . withArray args $ \carr ->
    _funcall env func argsLen carr
  where
    argsLen = fromIntegral (length args) :: CPtrdiff
