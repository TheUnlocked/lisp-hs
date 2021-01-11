{-# LANGUAGE LambdaCase, DerivingVia #-}

module Repl (
    repl
) where

import Stdlib
import Parser
import Interpreter
import Data.HashMap.Strict (keys)
import Data.List (isPrefixOf)
import System.Console.Haskeline
import System.Console.Haskeline.History
import LispTypes
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Data.Char

newtype ReplEnvironment a = ReplEnvironment (InputT Environment a)
    deriving (Monad, Applicative, Functor) via (InputT Environment)

replSettings :: Settings Environment
replSettings = Settings {
    autoAddHistory = False,
    historyFile = Nothing,
    complete = \(left, _) -> do
        scope <- getScope
        let (partial, restLeft) = break isBreaking left
            makeCompletion s = Completion { replacement = s, display = s, isFinished = False } 
        return (restLeft, makeCompletion <$> filter (isPrefixOf $ reverse partial) (keys $ bindings scope))
}

condenseWhitespace :: String -> String
condenseWhitespace str = case str of
    "" -> ""
    (w1 : w2 : rest) | isSpace w1 && isSpace w2 -> condenseWhitespace $ w2 : rest
    ('\n' : rest) -> condenseWhitespace $ ' ' : rest
    (c : rest) -> c : condenseWhitespace rest

findIndentation' :: [[LispToken]] -> [LispToken] -> [LispToken]
findIndentation' openPos tokens = case (openPos, tokens) of
    ([], []) -> []
    (pos : _, []) -> pos
    ([], (LispToken TokenParenR _) : rest) -> findIndentation' [] rest
    (_ : ps, (LispToken TokenParenR _) : rest) -> findIndentation' ps rest
    (ps, (LispToken TokenParenL _) : rest) -> findIndentation' (tokens : ps) rest
    (ps, _ : rest) -> findIndentation' ps rest

findIndentation :: [LispToken] -> Maybe Int
findIndentation tokens = case findIndentation' [] (filter (\case LispToken (TokenWhitespace _) _ -> False; _ -> True) tokens) of
    [] -> Nothing
    [paren] -> let LispToken _ ParseSource { col = c } = paren in Just $ c + 1
    (_ : x : (LispToken TokenEOL _) : _) -> let LispToken _ ParseSource { col = c } = x in Just c
    (_ : _ : x : _) -> let LispToken _ ParseSource { col = c } = x in Just c
    (_ : x : _) -> let LispToken _ ParseSource { col = c } = x in Just c

repl :: IO ()
repl = do
    runEnvironment env defaultBindings
    where
        env :: Environment ()
        env = runInputT replSettings runtime
        runtime :: InputT Environment ()
        runtime = forever $ do
            let getFullInput str = do
                    case findIndentation $ readTokens str of
                        Just i -> do
                            minput <- getInputLineWithInitial "| " (replicate i ' ', "")
                            getFullInput $ str ++ '\n' : fromMaybe "" minput
                        Nothing -> return str
            minput <- getInputLine "> "
            input <- getFullInput $ fromMaybe "" minput
            when (input /= "") $ modifyHistory $ addHistoryUnlessConsecutiveDupe $ condenseWhitespace input
            case readLisp input of
                Left errs -> liftIO $ sequence_ $ print <$> errs
                Right code -> do
                    result <- lift $ catchError (runStatements code) $ \case
                        RequestExit exitcode -> throwError $ RequestExit exitcode
                        err -> do
                            liftIO $ print err
                            return LispVoid
                    case result of 
                        LispVoid -> return ()
                        _ -> liftIO $ print result