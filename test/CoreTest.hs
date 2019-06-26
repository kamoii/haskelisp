{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings, TypeApplications, DataKinds #-}
module Main where

import Prelude()
import Protolude hiding (Symbol)
import Emacs.Core
import Emacs.Class
import Emacs.NAdvice (around')
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

foreign export ccall "emacs_module_init" emacsModuleInit :: EmacsModule

redisplayAdvice :: IORef Int -> Text -> EmacsM ()
redisplayAdvice ref fname = do
  around' fname $ \orig _ -> do
    preCount <- liftIO $ readIORef ref
    v <- orig
    postCount <- liftIO $ readIORef ref
    when (postCount - preCount > 0) $
      message $ "Redisplay while "
        <> fname
        <> " for "
        <> show (postCount - preCount) <> " times."
    pure v

data Frame = Frame
  { fEvald :: Bool
  , fFunc :: Text
  }

-- mapbacktrace の第二引数として nil を渡しているので mapbacktrace からスタックは始まる。
--
backtrace :: EmacsM [Frame]
backtrace = do
  ref <- liftIO $ newIORef []
  let cb :: Bool -> EmacsValue -> EmacsValue -> EmacsValue -> EmacsM ()
      cb evald funcObj args flags = do
        func <- readString . unsafeType =<< call1 "prin1-to-string" funcObj
        liftIO $ modifyIORef ref $ (Frame {fEvald = evald, fFunc = func} : )
        pure ()
  cbEv <- fun cb
  nil <- mkNil
  call2 "mapbacktrace" cbEv nil
  liftIO $ readIORef ref

addAdvicePostCommandHook
  :: IORef [Text]
  -> Handle
  -> EmacsM ()
addAdvicePostCommandHook ref handle = do
  hooksL :: [Symbol] <- getq "post-command-hook"
  hooksG :: [Symbol] <- getqDefault "post-command-hook"
  for_ (hooksL <> hooksG) $ \(Symbol sym) -> do
    ls <- liftIO $ readIORef ref
    when (sym /= "t" && not (elem sym ls)) $ do
      liftIO $ modifyIORef ref (sym:)
      around' sym $ \orig _ -> do
        liftIO $ do
          T.hPutStr handle $ "!!post command hook (" <> sym <> "): "
          hFlush handle
        v <- orig
        liftIO $ do
          T.hPutStrLn handle "DONE"
          hFlush handle
        pure v
  pure ()

logFunctionCall
  :: Handle
  -> Text
  -> EmacsM ()
logFunctionCall han fname = do
  around' fname $ \orig _ -> do
    liftIO $ do
      T.hPutStrLn han $ "!" <> fname
      hFlush han
    orig

-- どうなんだろ？どれぐらいから遅くなるんだろう？
-- 今のとこは何も問題はない気がする？
emacsModuleInit :: EmacsModule
emacsModuleInit = emacsModule (Just "core-test") $ do
  setValue "core-test-foo" =<< mkInteger 42
  setValue "core-test-bar" =<< mkString "日本語も問題ないはず"
  setFunction "core-test-append" =<< mkIOFun1 (readString. unsafeType) (pure . untype)
    (\str -> do
        mkString $ str <> " yeah!"
    )

  log <- liftIO $ openFile "/tmp/core-test.log" WriteMode
  liftIO $ T.hPutStrLn log "started" *> hFlush log

  setq "yeah" ("簡単に定義だ!" :: Text)
  setf "append-foo" $ \str -> str <> ("-foo" :: Text)
  setc "hello" InteractiveNoArgs $ do
    call1 "message" =<< mkString "world"

  redisplayCount <- liftIO $ newIORef 0

  -- redisplay の度に何回目の redisplay かをログに出力する
  re <- fun $ \(_ :: EmacsValue) -> do
    modifyIORef redisplayCount (+1)
    count <- readIORef redisplayCount
    T.hPutStrLn log $ "re: " <> show count
    hFlush log
  modifyq "pre-redisplay-functions" $ \(v :: EmacsValue) -> cons re v

  -- timer-idle-list のやつらを監視
  logFunctionCall log "show-paren-function"
  logFunctionCall log "jit-lock-context-fontify"
  logFunctionCall log "jit-lock-deferred-fontify"

  -- around' "run-hooks" $ \orig args -> do
  --   hooks <- traverse (readSymbol . unsafeType) args
  --   frames <- backtrace
  --   liftIO $ do
  --     T.hPutStrLn log $ "run-hook: " <> T.intercalate ", " (map show hooks)
  --     -- T.hPutStrLn log $ "            (" <> show (length frames) <> ") " <> T.intercalate ", " (map fFunc frames)
  --     hFlush log
  --   v <- orig
  --   pure v

  -- let advice name = around' name $ \orig (hookEv:xs) -> do
  --       hook <- readSymbol $ unsafeType hookEv
  --       frames <- backtrace
  --       liftIO $ do
  --         T.hPutStrLn log (name <> ": " <> show hook)
  --         -- T.hPutStrLn log $ show (length frames) <> ": " <> T.intercalate ", " (map fFunc frames)
  --         hFlush log
  --       v <- orig
  --       pure v
  -- advice "run-hook-with-args"
  -- advice "run-hook-with-args-until-failure"
  -- advice "run-hook-with-args-until-success"

  setc "redisplay-count" InteractiveNoArgs $ do
    count <- liftIO $ readIORef redisplayCount
    message $ show count
    map fFunc <$> backtrace

  aset <- liftIO $ newIORef []
  setc "set-post-command-hook-monitors" InteractiveNoArgs $ addAdvicePostCommandHook aset log


main :: IO ()
main = undefined
