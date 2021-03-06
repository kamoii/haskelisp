{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Emacs.Core
  ( EmacsValue
  , TypedEmacsValue
  , EmacsSymbol, EmacsString, EmacsInteger, EmacsFunction, EmacsCons
  , CallableEmacsValue(..)
  , IsEmacsValue
  , Doc(..), Arity(..)
  , unsafeType, untype
  , EmacsM
  , isEq
  , isNil
  , mkNil
  , mkString, readString
  , mkBool, readBool
  , mkInteger
  , Symbol(..), intern, mkSymbol, readSymbol, typeSymbol
  , Keyword(..), mkKeyword, unsafeReadKeyword
  , Cons(..), cons, mkCons, readCons, typeCons
  , mkList, unsafeReadList
  , mkAList
  , call', call, call1, call2, call3
  , mkFun
  , convFun1, convFun2
  , convIOFun1
  , mkFun1, mkFun2
  , mkIOFun1, mkIOFun2
  , ELisp(..), mkELisp
  , eval, evalLexical, evalString
  , message
  , emacsModule
  -- EmacsRuntime, CInt を export しないと foreign export ... EmacsModule で
  -- コンパイルエラーが発生する
  , EmacsModule, EmacsRuntime(..), CInt(..)
  -- Symbol操作
  , setValue, setFunction, setCommand, InteractiveForm(..)
  , getValue, getSymbolName, isBounded, getDefaultValue, setDefaultValue
  , getProperty, setProperty
  ) where

import Prelude(Show(..))
import Protolude hiding (Symbol)
import Emacs.Internal hiding (isNil, intern, emacsModule)
import qualified Emacs.Internal as I
import Foreign.Ptr (nullPtr)
import Foreign.StablePtr (castPtrToStablePtr)
import Foreign.C.Types (CInt(..))
import qualified Data.Text as T

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

throwException :: Text -> EmacsM a
throwException t = liftIO $ throwIO $ ErrorCall (toS t)

-- * 安全性について
--
--  unsafe が付いている操作は引数に EmacsValue を受けるようになってい
--  るが、ただしい型を渡すのはユーザの責任。付いてない関数は *意図して
--  いない型* であることは保証する。ただし型以外に原因により例外は発生
--  しうる(取る引数の数が異なる関数を渡した、など)。

typeByPredicate :: Text -> EmacsValue -> EmacsM (Maybe (TypedEmacsValue et))
typeByPredicate pred ev = do
  b <- isNil =<< call1 pred ev
  pure $ if b then Nothing else Just (unsafeType ev)

-- * Integer

mkInteger :: Int -> EmacsM (TypedEmacsValue EmacsInteger)
mkInteger = liftEM I.makeInteger

-- * String

mkString :: Text -> EmacsM (TypedEmacsValue EmacsString)
mkString = liftEM I.makeString

readString :: TypedEmacsValue EmacsString -> EmacsM Text
readString = liftEM I.extractString

-- * Symbol
-- Emacs の値に対する Haskell の型
-- 数値や文字列は素直なんだけど、他
-- Nil は空 [] でいいのかな？
newtype Symbol = Symbol Text
  deriving (Eq, Ord)

instance Show Symbol where show (Symbol s) = toS s

intern :: Text -> EmacsM (TypedEmacsValue EmacsSymbol)
intern = liftEM I.intern

mkSymbol :: Symbol -> EmacsM (TypedEmacsValue EmacsSymbol)
mkSymbol (Symbol name) = intern name

readSymbol :: TypedEmacsValue EmacsSymbol -> EmacsM Symbol
readSymbol ev = do
  name <- readString . unsafeType =<< call1 "symbol-name" ev
  pure $ Symbol name

typeSymbol :: EmacsValue -> EmacsM (Maybe (TypedEmacsValue EmacsSymbol))
typeSymbol = typeByPredicate "symbolp"

-- * Keyword
--
-- Emacs の内部的にはキーワードは : (コロン)から始まるシンボル。
-- keywordという型は存在しない。
-- 値として評価すると自身が返る。
newtype Keyword = Keyword Text

mkKeyword :: Keyword -> EmacsM EmacsValue
mkKeyword (Keyword kw) =
  untype <$> intern (":" <> kw)

unsafeReadKeyword :: EmacsValue -> EmacsM Keyword
unsafeReadKeyword ev = do
  name <- readString . unsafeType =<< call1 "symbol-name" ev
  when (not $ T.isPrefixOf ":" name) $ throwException "Not keyword"
  pure $ Keyword $ T.drop 1 name

-- * Bool
--
-- emacs は boolean型は存在しない。nilのみが false 扱いであり、残りは
-- true となる。ただし慣習的に t シンボル(値として自身を持つ)が true
-- として用いられる。

mkBool :: Bool -> EmacsM EmacsValue
mkBool b = if b then mkT else mkNil

readBool :: EmacsValue -> EmacsM Bool
readBool ev = not <$> isNil ev

-- * funcall: 関数呼び出しの効率化

-- apply/call
--
-- 最初は返値も ReadEmacsValue を用いて多相的に書いていたのだが、
--
--   * EmacsM () のために Nilの導入
--   * void' が必要に
--   * 自分の足を打ち抜きそうになるので
--
-- 多相的な結果型って何かアンチパターンの匂いがするんだけど、あまり言及見つからないな...

data CallableEmacsValue
  = CEVSymbol (TypedEmacsValue EmacsSymbol)
  | CEVFunction (TypedEmacsValue EmacsFunction)

instance IsEmacsValue CallableEmacsValue where
  toEv (CEVSymbol v) = untype v
  toEv (CEVFunction v) = untype v

call' :: CallableEmacsValue -> [EmacsValue] -> EmacsM EmacsValue
call' f args =
  join $ liftEM2 funcall (toEv f) <$> (pure args)

call :: Text -> [EmacsValue] -> EmacsM EmacsValue
call funcname args = do
  f <- intern funcname
  call' (CEVSymbol f) args

class IsEmacsValue ev where toEv :: ev -> EmacsValue
instance IsEmacsValue EmacsValue where toEv = identity
instance IsEmacsValue (TypedEmacsValue et) where toEv = untype

call1 :: IsEmacsValue a => Text -> a -> EmacsM EmacsValue
call1 fn ev0 = call fn [toEv ev0]

call2 :: (IsEmacsValue a, IsEmacsValue b) => Text -> a -> b -> EmacsM EmacsValue
call2 fn ev0 ev1 = call fn [toEv ev0, toEv ev1]

call3 :: (IsEmacsValue a, IsEmacsValue b, IsEmacsValue c) => Text -> a -> b -> c -> EmacsM EmacsValue
call3 fn ev0 ev1 ev2 = call fn [toEv ev0, toEv ev1, toEv ev2]

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


-- 関数

convFun1
  :: (EmacsValue -> EmacsM a0)
  -> (r -> EmacsM EmacsValue)
  -> (a0 -> r)
  -> ([EmacsValue] -> EmacsM EmacsValue)
convFun1 a0f rf f [a0] = rf =<< f <$> a0f a0

convFun2
  :: (EmacsValue -> EmacsM a0)
  -> (EmacsValue -> EmacsM a1)
  -> (r -> EmacsM EmacsValue)
  -> (a0 -> a1 -> r)
  -> ([EmacsValue] -> EmacsM EmacsValue)
convFun2 a0f a1f rf f [a0, a1] = rf =<< f <$> a0f a0 <*> a1f a1

convIOFun1
  :: LiftToEmacsM m
  => (EmacsValue -> EmacsM a)
  -> (r -> EmacsM EmacsValue)
  -> (a -> m r)
  -> ([EmacsValue] -> EmacsM EmacsValue)
convIOFun1 a0f rf f = convFun1 a0f (\a -> liftToEmacsM a >>= rf) f

mkFun1
  :: (EmacsValue -> EmacsM a)
  -> (r -> EmacsM EmacsValue)
  -> (a -> r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun1 a0f rf f = mkFun (Doc "") (Arity 1, Arity 1) $ convFun1 a0f rf f

mkFun2
  :: (EmacsValue -> EmacsM a0)
  -> (EmacsValue -> EmacsM a1)
  -> (r -> EmacsM EmacsValue)
  -> (a0 -> a1 -> r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkFun2 a0f a1f rf f = mkFun (Doc "") (Arity 2, Arity 2) $ convFun2 a0f a1f rf f

class LiftToEmacsM m where liftToEmacsM :: m a -> EmacsM a
instance LiftToEmacsM IO where liftToEmacsM = liftIO
instance LiftToEmacsM EmacsM where liftToEmacsM = identity

mkIOFun1
  :: LiftToEmacsM m
  => (EmacsValue -> EmacsM a)
  -> (r -> EmacsM EmacsValue)
  -> (a -> m r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkIOFun1 a0f rf f = mkFun1 a0f (\a -> liftToEmacsM a >>= rf) f

mkIOFun2
  :: LiftToEmacsM m
  => (EmacsValue -> EmacsM a0)
  -> (EmacsValue -> EmacsM a1)
  -> (r -> EmacsM EmacsValue)
  -> (a0 -> a1 -> m r)
  -> EmacsM (TypedEmacsValue EmacsFunction)
mkIOFun2 a0f a1f rf f = mkFun2 a0f a1f (\a -> liftToEmacsM a >>= rf) f

-- * Cons

data EmacsCons
data Cons = Cons EmacsValue EmacsValue

cons
  :: (IsEmacsValue car, IsEmacsValue cdr)
  => car
  -> cdr
  -> EmacsM (TypedEmacsValue EmacsCons)
cons car cdr = mkCons $ Cons (toEv car) (toEv cdr)

mkCons :: Cons -> EmacsM (TypedEmacsValue EmacsCons)
mkCons (Cons car cdr) = unsafeType <$> call2 "cons" car cdr

readCons :: TypedEmacsValue EmacsCons -> EmacsM Cons
readCons (TypedEmacsValue ev) =
  liftA2 Cons (call1 "car" ev) (call1 "cdr" ev)

typeCons :: EmacsValue -> EmacsM (Maybe (TypedEmacsValue EmacsCons))
typeCons = typeByPredicate "consp"

-- * ELisp, eval/evalString
--
-- 呼び出しが複数必要な場合、call/mk*系よりは簡単に使える。
-- またマクロ系は funcall では呼べないので eval経由で呼ぶ必要がある。
-- 型安全のへったくりもないので注意して使う必要あり。
--

data ELisp
  = ESymbol Text
  | EString Text
  | EKeyword Text
  | EInt Int
  | ECons ELisp ELisp
  | EList [ELisp]
  | EAlist [(ELisp,ELisp)]
  | ENil
  | EEmacsValue EmacsValue

mkELisp :: ELisp -> EmacsM EmacsValue
mkELisp (ESymbol sym) = untype <$> mkSymbol (Symbol sym)
mkELisp (EString str) = untype <$> mkString str
mkELisp (EKeyword kw) = mkKeyword (Keyword kw)
mkELisp (EInt i) = untype <$> mkInteger i
mkELisp (ECons car cdr) = untype <$> join (cons <$> mkELisp car <*> mkELisp cdr)
mkELisp (EList ls) = mkList =<< traverse mkELisp ls
mkELisp (EAlist as) = mkAList =<< traverse (\(car,cdr) -> (,) <$> mkELisp car <*> mkELisp cdr) as
mkELisp ENil = mkNil
mkELisp (EEmacsValue ev) = pure ev

eval :: ELisp -> EmacsM EmacsValue
eval sexp =
  call1 "eval" =<< mkELisp sexp

evalLexical :: [(ELisp, ELisp)] -> ELisp -> EmacsM EmacsValue
evalLexical binds sexp = do
  bindsEv <- mkELisp $ EAlist binds
  sexpEv <- mkELisp sexp
  call2 "eval" sexpEv bindsEv

evalString :: Text -> EmacsM EmacsValue
evalString str = do
  q <- call1 "car" =<< call1 "read-from-string" =<< mkString str
  call1 "eval" q

-- nil 型は存在しない
-- また nil 値はプロパティを持っていないため、opaque な値を用意する必要はない。
-- 単一の値しかないので引数は不要。どうやって取得するだろ？
-- nil という定数が nil を持っている。
-- (symbol-value 'nil) でいけるかな。(eval 'nil) でもいいかも
--
-- TODO: キャッシュするべきだよね(キャッシュする場合は emacs_value を
-- emacs側でGCされないように global_ref を作る必要があるのかな？
mkNil :: EmacsM EmacsValue
mkNil = call1 "symbol-value" =<< intern "nil"

-- Emacs には boolean型は存在しない。
-- nil が false、nil以外が true と見做される。
-- ただ慣習として t シンボルが true の値として良く利用される。
mkT :: EmacsM EmacsValue
mkT = call1 "symbol-value" =<< intern "t"

-- そもそも list という型は emacs側には存在しない。
-- listp という関数があるが、これは cons もしくは nil かどうかを判定している。
mkList :: [EmacsValue] -> EmacsM EmacsValue
mkList evs = call "list" evs

mkAList :: [(EmacsValue,EmacsValue)] -> EmacsM EmacsValue
mkAList alist = do
  evs <- traverse mkCons $ map (\(a,b) -> Cons a b) alist
  mkList (untype <$> evs)

unsafeReadList :: EmacsValue -> EmacsM [EmacsValue]
unsafeReadList ev = do
  b <- isNil ev
  if b
    then pure []
    else do
      h <- call1 "car" ev
      t <- call1 "cdr" ev
      (h:) <$> unsafeReadList t

-- * Module

message :: Text -> EmacsM ()
message txt =
  void $ call1 "message" =<< mkString txt

emacsModule :: Maybe Text -> EmacsM () -> EmacsModule
emacsModule modnameMaybe act =
  I.emacsModule $ \env -> runEmacsM env $ do
    act
    case modnameMaybe of
      Nothing -> pure ()
      Just modname -> void $ call1 "provide" =<< intern modname

-- * set系

setValue :: IsEmacsValue a => Text -> a -> EmacsM EmacsValue
setValue name v = do
  nameQ <- intern name
  call2 "set" nameQ v

--  関数の設定
-- 一番 low level なのが setFunction
setFunction :: Text -> TypedEmacsValue EmacsFunction -> EmacsM ()
setFunction name f = do
  nameQ <- intern name
  void $ call2 "fset" nameQ f

-- 依存型とか駆使すれば多分 interacvie form と関数の引数の型の整合性が
-- 取れていることが多分保証できる。

-- これは直接は使って欲くないため、' postfix を付けている。
-- EmacsValue を引数として取っているので function 以外のものを渡せてしまう
--
-- interactive-form はシンボル側及び関数側でも独立に情報を持つようだ。
-- 例えば (lambda (str) (interactive "s") (message str)) とか。
-- これはシンボル側の 属性設定からは影響を受けない。
-- 関数の interactive-form は作成後は変更できない模様(少なくとも関数はない)
--
--  * symbol から起動する場合はどっちかに interactive-form が設定されていれば OK
--    - 両方設定されている場合は symbol 側の interactive-form 属性が優先される模様
--    - 上記の挙動については `interactive-form` C関数のコード見れば、コメントで記載されている
--  * 直接関数オブジェクトから起動する場合は その関数側に interactive-form が設定されている必要がある。
--
-- (defun hoge (str) (interactive "s") ...) のように定義した場合は関
-- 数側に interactive-form が設定されるので、シンボル側の
-- interactive-form 属性は nil のまま。
--
-- > (get 'hoge 'interactive-form) ;=> nil
-- > (interactive-form 'hoge)      ;=> "s"
-- > (put  'hoge 'interactive-form '(interactive "s\ns\n"))  ;シンボル側に設定
-- > (interactive-form 'hoge)      ;=> "s\ns\n"   シンボル側優先

data InteractiveForm
  = InteractiveNoArgs

-- 2018/01/24
-- この方式はうまくいかない？
-- setCommand
--   :: Text
--   -> InteractiveForm
--   -> (TypedEmacsValue EmacsFunction)
--   -> EmacsM ()
-- setCommand fname form f = do
--   fnameQ <- intern fname
--   interactiveFormQ <- intern "interactive-form"
--   void $ call2 "fset" fnameQ f
--   void $ call3 "put"  fnameQ interactiveFormQ =<< evalString "'(interactive)"

-- eval の lexical binding を利用
--   (lambda (&rest args)
--     (interactive)
--     (apply f args))
setCommand
  :: Text
  -> InteractiveForm
  -> (TypedEmacsValue EmacsFunction)
  -> EmacsM ()
setCommand fname form f = do
  let binds = [(ESymbol "f", EEmacsValue (untype f))]
  let sexp = EList [ ESymbol "lambda"
                   , EList [ESymbol "&rest", ESymbol "args"]
                   , EList [ESymbol "interactive"]
                   , EList [ESymbol "apply", ESymbol "f", ESymbol "args"]
                   ]
  setFunction fname =<< unsafeType <$> evalLexical binds sexp


-- Symbol has four slots.
--
--  1. name
--  2. value (* can have buffer local value)
--  3. function
--  4. property list (* can have buffer local list)

getSymbolName :: Text -> EmacsM Text
getSymbolName name = do
  name <- call1 "symbol-name" =<< intern name
  readString $ unsafeType name

-- Could throw exception if the symbol is not setted.
getValue :: Text -> EmacsM EmacsValue
getValue name =
  call1 "symbol-value" =<< intern name

-- if Symbol exists (included in obarray) and a value is bounded.
isBounded :: Text -> EmacsM Bool
isBounded name =
  readBool =<< call1 "boundp" =<< intern name

-- Buffer local
--
-- If the variable is buffer local, you need to use
-- getDefaultValue/setDefaultValue to set/get the global variable.
getDefaultValue :: Text -> EmacsM EmacsValue
getDefaultValue name =
  call1 "default-value" =<< intern name

setDefaultValue :: IsEmacsValue a => Text -> a -> EmacsM EmacsValue
setDefaultValue name val = do
  nameQ <- intern name
  call2 "set-default" nameQ val

--  Keyword Symbol

--  シンボルは任意の属性を持つことができる。
--
-- 属性テーブルは シンボルと任意の値に間のハッシュである。
-- ただし値として nil は設定できない。未設定とnil に設定は区別されない。
getProperty :: Text -> Text -> EmacsM (Maybe EmacsValue)
getProperty name property = do
  nameQ <- intern name
  propertyQ <- intern property
  ev <- call2 "get" nameQ propertyQ
  b <- not <$> isNil ev
  return $ if b then Just ev else Nothing

setProperty
  :: (IsEmacsValue v)
  => Text
  -> Text
  -> v
  -> EmacsM EmacsValue
setProperty name property value = do
  nameQ <- intern name
  propertyQ <- intern property
  call3 "put" nameQ propertyQ value
