module Emacs.Internal where

import Prelude()
import Protolude
import Emacs.Type
import Emacs.Core

-- ??何だこれ。
-- PersistentState といいたかったのかな？
-- ShareState のほうがいいかな？
--
-- A stable pointer is a reference to a Haskell expression that is
-- guaranteed not to be affected by garbage collection, i.e., it will
-- neither be deallocated nor will the value of the stable pointer
-- itself change during garbage collection (ordinary references may be
-- relocated during garbage collection). Consequently, stable pointers
-- can be passed to foreign code, which can treat it as an opaque
-- reference to a Haskell value.
--
--  * 指している対象がGCの影響を受けない
--  * リロケートもされない
--  * そのため foreign code に渡すことが可能
--
data PState = PState
  { symbolMap :: IORef (Map Text GlobalEmacsValue)
  }

-- Stable Ptr は
data Ctx = Ctx
  { pstateStablePtr :: StablePtr PState
  , pstate :: PState
  , emacsEnv :: EmacsEnv
  }

type EmacsM =
  ReaderT Ctx IO

type EmacsModule = Ptr () -> IO CInt

-- 基本的に一度作成したコンテキストを使い回す。
-- ただし現状では別途コンテキストを作成して使ったとしても問題ない
-- initCtx :: MonadIO m => EmacsEnv -> m Ctx
-- initCtx env = do
--   pstate <- initState
--   pstatep <- liftIO $ newStablePtr pstate
--   return $ Ctx pstatep pstate env
--   where
--     initState :: MonadIO m => m PState
--     initState = do
--       mapRef <- liftIO $ newIORef mempty
--       return $ PState mapRef
-- 
-- getPStateStablePtr :: EmacsM (StablePtr PState)
-- getPStateStablePtr = pstateStablePtr <$> ask
-- 
-- getPState :: EmacsM PState
-- getPState = pstate <$> ask
-- 
-- getEnv :: EmacsM EmacsEnv
-- getEnv = emacsEnv <$> ask

-- Logging here is not a good idea. When passing high order function,
-- which could be invoked manytimes, its get quite slow.
-- runEmacsM :: MonadIO m => Ctx -> EmacsM a -> m a
-- runEmacsM ctx action =
--   liftIO $ runReaderT action ctx

-- -- 関数の引数に ToEmacsValue を受け取るようにすると便利なんだけど、問
-- -- 題はその引数の実際の値を取得するめに EmacsM の中で実行する必要があ
-- -- り、引数の実行で例外が発生するかもしれない、ということ。
-- --
-- -- ある関数の中でEmacsException例外が発生したときにどのタイミングでど
-- -- こまで進んだかの保証が得られない。
-- -- ああ、けど IO での例外でも同じことが言えるのか...
--
-- -- fromTypedEmacsValue が実装できないものもある。
-- class CheckTypeEmacsValue (ET t) => ToTypedEmacsValue t where
--   type ET t :: EmacsType
--
--   toTypedEmacsValue :: t -> EmacsM (TypedEmacsValue (ET t))
--
--   toEmacsValue :: t -> EmacsM (EmacsValue)
--   toEmacsValue t = do
--     TypedEmacsValue ev :: TypedEmacsValue (ET t) <- toTypedEmacsValue t
--     return ev
--
--   toEv :: t -> EmacsM (EmacsValue)
--   toEv = toEmacsValue
--
--   fromTypedEmacsValue :: TypedEmacsValue (ET t) -> EmacsM t
--
--   fromEmacsValue :: EmacsValue -> EmacsM t
--   fromEmacsValue ev =
--     fromTypedEmacsValue =<< typeCheck ev
--
--   fromEv :: EmacsValue -> EmacsM t
--   fromEv = fromEmacsValue
--
-- -- 便宜上
-- instance ToTypedEmacsValue EmacsValue where
--   type ET EmacsValue = 'EUnkown
--   toEmacsValue = pure
--   toTypedEmacsValue = pure . TypedEmacsValue
--   fromEmacsValue = pure
--   fromTypedEmacsValue = pure . untype
--
-- -- 便宜上
-- instance CheckTypeEmacsValue et => ToTypedEmacsValue (TypedEmacsValue et) where
--   type ET (TypedEmacsValue et) = et
--   toTypedEmacsValue = pure
--   fromTypedEmacsValue = pure
--
-- instance ToTypedEmacsValue Int where
--   type ET Int = 'EInteger
--   toTypedEmacsValue = mkInteger
--   fromTypedEmacsValue = extractInteger
--
-- instance ToTypedEmacsValue Text where
--   type ET Text = 'EString
--   toTypedEmacsValue = mkString
--   fromTypedEmacsValue = extractString
--
-- instance ToTypedEmacsValue Keyword where
--   type ET Keyword = 'EKeyword
--   toTypedEmacsValue (Keyword t) =
--     TypedEmacsValue . untype <$> intern (":" <> t)
--
-- instance ToTypedEmacsValue Symbol where
--   type ET Symbol = 'ESymbol
--   toTypedEmacsValue (Symbol t) = intern t
--
-- instance Functionable f => ToTypedEmacsValue (Function f) where
--   type ET (Function f) = 'EFunction
--   toTypedEmacsValue (Function f) = mkFunctionFromFunctionable f
--
-- instance (FromEmacsValue a, Functionable b) => ToTypedEmacsValue (a -> b) where
--   type ET (a -> b) = 'EFunction
--   toTypedEmacsValue = mkFunctionFromFunctionable
--
-- instance (ToTypedEmacsValue a,ToTypedEmacsValue b) => ToTypedEmacsValue (a,b) where
--   type ET (a,b) = 'ECons
--   toTypedEmacsValue (a,b) = do
--     av <- toTypedEmacsValue a
--     bv <- toTypedEmacsValue b
--     mkCons av bv
--
-- instance ToEmacsValue x => ToTypedEmacsValue [x] where
--   type ET [x] = 'EList
--   toTypedEmacsValue xs = join $ mkList <$> mapM toEv xs
--
-- instance ToTypedEmacsValue () where
--   type ET () = 'ENil
--   toTypedEmacsValue _ = mkNil
--
-- instance ToTypedEmacsValue Bool where
--   type ET Bool = 'EBool
--   toTypedEmacsValue True  = mkT
--   toTypedEmacsValue False = TypedEmacsValue . untype <$> mkNil
--
-- type ToEmacsValue t = ToTypedEmacsValue t
-- type ToEmacsSymbol t = (ToTypedEmacsValue t, ET t ~ 'ESymbol)
-- type ToEmacsInteger t = (ToTypedEmacsValue t, ET t ~ 'EInteger)
-- type ToEmacsString t = (ToTypedEmacsValue t, ET t ~ 'EString)
-- type ToEmacsKeyword t = (ToTypedEmacsValue t, ET t ~ 'EKeyword)
-- type ToEmacsFunction t = (ToTypedEmacsValue t, ET t ~ 'EFunction)
--
-- type FromEmacsValue t = ToTypedEmacsValue t

-- -- EmacsValue -> TypedEmacsValue et
-- -- 排他的ではないことに注意
-- -- 変換できない場合は例外を投げる
-- -- TODO: type-of を使ったほうが早いかな？
-- class CheckTypeEmacsValue (et :: EmacsType) where
--   isTypeOf :: Proxy et -> EmacsValue -> EmacsM Bool
--   typeCheck :: EmacsValue -> EmacsM (TypedEmacsValue et)
-- 
-- typeCheckWithP :: Text -> EmacsValue -> EmacsM (TypedEmacsValue et)
-- typeCheckWithP p ev = do
--   b <- isNil =<< funcall1 p ev
--   if b
--     then error $ toS $ "type error with predicate function: " <> p
--     else pure $ TypedEmacsValue ev
-- 
-- -- opaque なまあ扱う。チェックは不要
-- instance CheckTypeEmacsValue 'EUnkown where
--   typeCheck = pure . TypedEmacsValue
-- 
-- instance CheckTypeEmacsValue 'ESymbol where
--   typeCheck = typeCheckWithP "symbolp"
-- 
-- instance CheckTypeEmacsValue 'EString where
--   typeCheck = typeCheckWithP "stringp"
-- 
-- instance CheckTypeEmacsValue 'EInteger where
--   typeCheck = typeCheckWithP "integerp"
-- 
-- instance CheckTypeEmacsValue 'ENil where
--   typeCheck ev =
--     isNil ev >>= bool (error "nil type error") (pure $ TypedEmacsValue ev)
-- 
-- instance CheckTypeEmacsValue 'EFunction where
--   typeCheck = typeCheckWithP "functionp"
-- 
-- instance CheckTypeEmacsValue 'ECons where
--   typeCheck = typeCheckWithP "consp"
-- 
-- instance CheckTypeEmacsValue 'EKeyword where
--   typeCheck = typeCheckWithP "keywordp"
-- 
-- -- TODO: どうするかな？
-- instance CheckTypeEmacsValue 'EBool where
--   typeCheck = undefined
-- 
-- -- TODO: listp は (1 . 2) とかでも t 返す。なので時前で確認する必要あり
-- instance CheckTypeEmacsValue 'EList where
--   typeCheck = typeCheckWithP "listp"

--
-- class FromEmacsValue h where
--   fromEv :: EmacsValue -> EmacsM h

-- instance FromEmacsValue EmacsValue where
--   fromEv = pure

-- String は ToEmacsValue との対象性の観点から止めたほうがよい。
-- String への変換が必要なのであれば先にTextに変換してからだな。
-- instance FromEmacsValue String where
--   fromEv = (toS<$>) . extractString

-- -- 多相的な関数は駄目らしい(具体的な関数ならokらしい)
-- -- TODO: optional, rest 引数に対応する。
-- class Functionable a where
--     call :: a -> [EmacsValue] -> EmacsM (Either Text EmacsValue)
--     arity :: a -> Int
-- 
-- instance {-# OVERLAPPING #-} ToEmacsValue a => Functionable a where
--     call a [] = Right <$> toEv a
--     call _ _  = pure $ Left "Too many arguments"
--     arity _ = 0
-- 
-- instance {-# OVERLAPPING #-} ToEmacsValue a => Functionable (IO a) where
--     call a [] = do
--       v <- liftIO a
--       Right <$> toEv v
--     call _ _  = pure $ Left "Too many arguments"
--     arity _ = 0
-- 
-- instance {-# OVERLAPPING #-} ToEmacsValue a => Functionable (EmacsM a) where
--     call am [] = do
--       a <- am
--       Right <$> toEv a
--     call _ _  = pure $ Left "Too many arguments"
--     arity _ = 0
-- 
-- instance {-# OVERLAPPING #-} (FromEmacsValue a, Functionable b) => Functionable (a -> b) where
--   call f (e:es) = do
--     av <- fromEv e
--     call (f av) es
--   call _ [] = pure $ Left "Too less arguments"
--   arity f = arity (f undefined) + 1

-- 多相的な関数は怒られるはず。
mkFunctionFromFunctionable :: Functionable f => f -> EmacsM EmacsFunction
mkFunctionFromFunctionable f = do
  let a = arity f
  mkFunction func a a ""
  where
    func :: [EmacsValue] -> EmacsM EmacsValue
    func es = do
      res <- call f es
      case res of
        Right ev -> return ev
        Left _   -> undefined

-- -- Symbol or a Function
-- -- ただ シンボルに functoin が設定されているかどうかまでは確認しない。
-- -- Text も特別に便宜のため対応させる -> OverloadedString と相性が悪いので止め
class EmacsCallable c where
  toCallableEmacsValue :: c -> EmacsM EmacsValue
-- instance EmacsCallable Symbol
-- instance EmacsCallable (TypedEmacsValue EmacsSymbol)
-- instance EmacsCallable (TypedEmacsValue EmacsFunction)

-- Function call Utilities
-- 多くの場合、文字列で関数を起動したいため、'なしでは Text で起動できるようにする。
funcall1'
  :: (EmacsCallable f,ToEmacsValue a)
  => f
  -> a
  -> EmacsM EmacsValue
funcall1' f ev0 =
  join $ funcall <$> toEv f <*> sequence [toEv ev0]

funcall1
  :: ToEmacsValue a
  => Text
  -> a
  -> EmacsM EmacsValue
funcall1 fname ev0 =
  funcall1' (Symbol fname) ev0

funcall2'
  :: (EmacsCallable f,ToEmacsValue a, ToEmacsValue b)
  => f
  -> a
  -> b
  -> EmacsM EmacsValue
funcall2' f ev0 ev1 =
  join $ funcall <$> toEv f <*> sequence [toEv ev0, toEv ev1]

funcall2
  :: (ToEmacsValue a, ToEmacsValue b)
  => Text
  -> a
  -> b
  -> EmacsM EmacsValue
funcall2 fname ev0 ev1 =
  funcall2' (Symbol fname) ev0 ev1

funcall3
  :: (ToEmacsValue a, ToEmacsValue b, ToEmacsValue c)
  => Text
  -> a
  -> b
  -> c
  -> EmacsM EmacsValue
funcall3 fname ev0 ev1 ev2 =
  join $ funcall <$> (untype <$> intern fname)
                 <*> sequence [toEv ev0, toEv ev1, toEv ev2]

callInteractively :: Text -> EmacsM EmacsValue
callInteractively fname =
  funcall1 "call-interactively" (Symbol fname)


-- 単一の値しかないので引数は不要。どうやって取得するだろ？
-- nil という定数が nil を持っている。
-- (symbol-value 'nil) でいけるかな。(eval 'nil) でもいいかも
--
-- TODO: キャッシュするべきだよね(キャッシュする場合は emacs_value を
-- emacs側でGCされないように global_ref を作る必要があるのかな？
mkNil :: EmacsM EmacsNil
mkNil =
  TypedEmacsValue <$> funcall1 "symbol-value" (Symbol "nil")

mkT :: EmacsM EmacsBool
mkT =
  TypedEmacsValue <$> funcall1 "symbol-value" (Symbol "t")

-- そもそも list という型は emacs側には存在しない。
-- listp という関数があるが、これは cons もしくは nil かどうかを判定している。
mkList :: [EmacsValue] -> EmacsM EmacsList
mkList evs = do
  listQ <- intern "list"
  TypedEmacsValue <$> funcall (untype listQ) evs

provide :: ToEmacsSymbol s => s -> EmacsM ()
provide feature =
  void $ funcall1 "provide" feature

message :: Text -> EmacsM ()
message t =
  void $ funcall1 "message" t

print :: ToEmacsValue v => v -> EmacsM ()
print ev =
  void $ funcall1 "print" ev

mkCons
  :: (ToEmacsValue a, ToEmacsValue b)
  => a
  -> b
  -> EmacsM EmacsCons
mkCons a b =
  TypedEmacsValue <$> funcall2 "cons" a b

car :: EmacsCons -> EmacsM EmacsValue
car = funcall1 "car"

cdr :: EmacsCons -> EmacsM EmacsValue
cdr = funcall1 "cdr"

-- `read-from-string` が失敗した場合は例外投げるので cons かどうかの確
-- 認は不要。
evalString :: Text -> EmacsM EmacsValue
evalString t =
  funcall1 "eval" =<< (car . typeUnsafe =<< funcall1 "read-from-string" t)

-- emacsModuleInit に渡す関数
-- TODO: move to Module.hs
defmodule :: Text -> EmacsM a -> EmacsModule
defmodule name mod ert = do
  env <- getEmacsEnvFromRT ert
  errorHandle env $ do
    ctx <- initCtx env
    runEmacsM ctx $ mod >> funcall1 "provide" (Symbol name)
  return 0

-- TODO: キャッシュするのは不味い気がしてきた。滅多にないとは思うけど、
-- unintern された場合にの動きが問題となる。
-- intern :: Text -> EmacsM (TypedEmacsValue EmacsSymbol)
-- intern str = do
--   s' <- lookupCache
--   TypedEmacsValue <$> case s' of
--     Just gev ->
--       return (castGlobalToEmacsValue gev)
--     Nothing ->
--       storeToCache =<< create
--   where
--     lookupCache = do
--       mapRef <- symbolMap <$> getPState
--       map <- liftIO $ readIORef mapRef
--       return $ Map.lookup str map
-- 
--     -- TODO: 現在は全部入れているけど、これはまずい
--     storeToCache ev = do
--       mapRef <- symbolMap <$> getPState
--       gev <- mkGlobalRef ev
--       liftIO $ modifyIORef mapRef (Map.insert str gev)
--       return (castGlobalToEmacsValue gev)
-- 
--     create = do
--       env <- getEnv
--       checkExitStatus . withCString (toS str) $ \cstr -> _intern env cstr
