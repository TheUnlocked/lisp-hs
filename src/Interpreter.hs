module Interpreter (
    runStatements,
    eval
) where

import Control.Monad.Supply
import Control.Monad.State
import Data.HashMap.Strict ( HashMap, fromList, (!?) )
import qualified Data.List.Safe as Safe

import LispTypes
import Parser
import Data.Functor
import Data.Maybe


newScope :: ScopeBindings -> LispScope -> LispScope
newScope bindings oldScope = LispScope { parentScope = Just oldScope, bindings = bindings }

findInScope :: String -> Environment LispObject
findInScope str = do
    currScope <- getScope
    findInScope' currScope where
    findInScope' LispScope { parentScope = parent, bindings = bindings } =
        case (bindings !? str, parent) of
            (Just x, _) -> return x
            (Nothing, Just nextScope) -> findInScope' nextScope
            (Nothing, _) -> throwError $ NotBoundInScope str

runStatements :: [LispExpr] -> Environment LispObject
runStatements statements = do
    results <- sequence $ eval <$> statements
    return $ fromMaybe LispVoid . Safe.last $ results

eval :: LispExpr -> Environment LispObject
eval expr = case expr of
    LispNil -> throwError InvokingEmptyList
    (LispPair a b) -> do
        fn <- eval a
        runLispFn fn b
    (LispString str) -> do
        ident <- supplyIdent
        return $ LispStringObject ident str
    (LispSymbol str) -> findInScope str
    (LispRational v) -> return $ LispRationalObject v
    (LispDouble v) -> return $ LispDoubleObject v

runLispFn :: LispObject -> LispExpr -> Environment LispObject
runLispFn LispFunction {fnParams = params, fnVariadicParam = vparam, fnBody = body, fnScope = scope} args = do
    bindings <- generateBindings params vparam args
    runIn (newScope bindings scope) (runStatements body)
runLispFn (LispFunctionBuiltin _ fn) args' = do
    args <- tryFromLispList args' (BadSyntax $ "Expected argument list, got " ++ show args')
    argList <- sequence $ eval <$> args
    fn argList
runLispFn LispMacro {macroParam = param, macroBody = body, macroScope = scope} args = do
    arg <- toLispObject args
    let bindings = fromList [(param, arg)]
    macroOutput <- runIn (newScope bindings scope) (runStatements body)
    case fromLispObjectList macroOutput of
        Just statementObjects -> do
            statements <- sequence $ fromLispObject <$> statementObjects
            runStatements statements
        Nothing -> throwError $ NotExpandable LispVoid
runLispFn (LispMacroBuiltin _ fn) args =
    fn args
runLispFn v _ =
    throwError $ NotInvokable v


tryFromLispList :: LispExpr -> RuntimeError -> Environment [LispExpr]
tryFromLispList x err = case fromLispList x of
    Just x  -> return x
    Nothing -> throwError err

generateBindings :: [String] -> Maybe String -> LispExpr -> Environment ScopeBindings
generateBindings params vparam' args' = do
    args <- tryFromLispList args' (BadSyntax $ "Expected argument list, got " ++ show args')
    evaledArgs <- sequence $ eval <$> args
    let ctParams = length params
        ctArgs   = length args
    case vparam' of
        Just vparam ->
            if ctArgs < ctParams then
                throwError $ WrongArgumentCount ctParams ctArgs
            else do
                let (namedArgs, vargs') = splitAt ctParams evaledArgs
                vargs <- toLispObjectList vargs'
                return $ fromList $ (vparam, vargs) : zip params namedArgs
        Nothing ->
            if ctArgs /= length params then
                throwError $ WrongArgumentCount ctParams ctArgs
            else
                return $ fromList (zip params evaledArgs)