{-# LANGUAGE LambdaCase, TupleSections #-}

module Stdlib (
    runCode,
    defaultBindings
) where

import Data.HashMap.Strict (keys, union, insert, fromList, HashMap)
import qualified Data.HashMap.Strict as HashMap (lookup)
import Control.Monad
import Control.Monad.State
import qualified Data.List.Safe as Safe
import Interpreter
import Parser
import LispTypes
import Util.Maybe
import Data.Maybe
import GHC.Real
import Data.Functor


runCode :: String -> IO ()
runCode str = do
    let read' = readLisp str
    case read' of
        Left x -> print x
        Right expr -> runEnvironment (runStatements expr) defaultBindings


macroArgsFromLispList :: LispExpr -> Environment [LispExpr]
macroArgsFromLispList x = case fromLispList x of
    Just x  -> return x
    Nothing -> throwError $ BadSyntax ""


type Expectation = LispObject -> Maybe (LispObject, String)

renameExpectation :: String -> Expectation -> Expectation
renameExpectation name exp = liftM2 (>>) exp (\x -> Just (x, name))

isEither :: String -> [Expectation] -> Expectation
isEither name exps = liftM2 (>>) (foldr1 (liftM2 (>>)) exps) (\x -> Just (x, name))

isAll :: String -> [Expectation] -> Expectation
isAll name exps = liftM2 (>>) (foldr1 (liftM2 mplus) exps) (\x -> Just (x, name))

isList :: Expectation
isList x   = case x of LispNilObject -> Nothing; LispPairObject _ _ xs -> isList xs; _ -> Just (x, "is-list")

isPair :: Expectation
isPair     = \case LispPairObject {} -> Nothing; x -> Just (x, "is-pair")

isRational :: Expectation
isRational = \case LispRationalObject _ -> Nothing; x -> Just (x, "is-rational")

isDouble :: Expectation
isDouble   = \case LispDoubleObject _ -> Nothing; x -> Just (x, "is-double")

isNumeric :: Expectation
isNumeric  = "is-numeric" `isEither` [isRational, isDouble]

isAny :: Expectation
isAny = const Nothing

expectPattern :: String -> [Expectation] -> Expectation
expectPattern name exps x = case fromLispObjectList x of
    Just xs -> foldr1 mplus exps' >>= (\(x, _) -> Just (x, name)) where
        exps' = zipWith (\exp x -> exp x) exps xs
    Nothing -> Just (x, name)

guardPattern :: [Expectation] -> ([LispObject] -> Environment LispObject) -> [LispObject] -> Environment LispObject
guardPattern exps fn xs = do
    if length exps /= length xs then
        throwError $ WrongArgumentCount (length exps) (length xs)
    else
        let exps' = zipWith (\exp x -> exp x) exps xs in
        case foldr1 mplus exps' of
            Just (failedOn, expected) -> throwError $ ExpectViolation expected failedOn
            Nothing -> fn xs


defaultBindings :: Environment ScopeBindings
defaultBindings = do {
    let (names, objs') = unzip binds
    ; objs <- sequence objs'
    ; let mappings = fromList $ zip names objs
          aliasMappings = fromList $ concat $ aliases <&> \(name, aliases') -> (, fromJust $ HashMap.lookup name mappings) <$> aliases'
    ; return $ union mappings aliasMappings
} where
    binds = [
            ("lambda"       , mkMacro lambda),
            ("define"       , mkMacro define),
            ("define-macro" , mkMacro defineMacro),
            ("quote"        , mkMacro quote),
            ("display"      , mkFn  $ guardPattern [isAny] display),
            ("+"            , mkFn  $ guardPattern [isNumeric, isNumeric] add),
            ("-"            , mkFn  $ guardPattern [isNumeric, isNumeric] subtract),
            ("*"            , mkFn  $ guardPattern [isNumeric, isNumeric] multiply),
            ("/"            , mkFn  $ guardPattern [isNumeric, isNumeric] divide),
            ("pow"          , mkFn  $ guardPattern [isNumeric, isNumeric] power),
            ("cons"         , mkFn  $ guardPattern [isAny, isAny] cons),
            ("car"          , mkFn  $ guardPattern [isPair] car),
            ("cdr"          , mkFn  $ guardPattern [isPair] cdr),
            ("list"         , mkFn    list),
            ("quit"         , mkFn    quit)
        ]
    aliases = [
            ("quit", ["exit"]),
            ("car", ["head", "first"]),
            ("cdr", ["tail", "rest"])
        ]
    mkFn x = do
        ident <- supplyIdent
        return $ LispFunctionBuiltin ident x
    mkMacro x = do
        ident <- supplyIdent
        return $ LispMacroBuiltin ident x

    quote args' = do
        args <- macroArgsFromLispList args'
        case args of
            [arg] -> toLispObject arg
            _ -> throwError $ BadSyntax "quote takes one argument"

    display [arg] = do
        liftIO $ print arg
        return LispVoid

    lambda' args = do
        currScope <- getScope
        case fromLispList' $ head args of
            (params, vparam) -> do
                let symbolMaybe :: LispExpr -> Maybe String
                    symbolMaybe = (\case LispSymbol x -> Just x; _ -> Nothing)
                case liftM2 (,) (sequence $ symbolMaybe <$> params) (flipMaybe $ symbolMaybe <$> vparam) of
                    Just (reifiedParams, reifiedVparam) -> do
                        ident <- supplyIdent
                        let fnBody = tail args
                        return $ LispFunction ident reifiedParams reifiedVparam fnBody currScope
                    Nothing -> throwError $ BadSyntax "The parameters of a function must be valid symbols"

    lambda args' = do
        args <- macroArgsFromLispList args'
        lambda' args

    define args' = do
        currScope <- getScope
        args <- macroArgsFromLispList args'
        if length args < 2 then
            throwError $ BadSyntax "define requires at least a name and a value"
        else
            case head args of
                LispSymbol name -> do
                    val <- eval $ args !! 1
                    setScope currScope { bindings = insert name val $ bindings currScope }
                LispPair _ _ -> do
                    case fromLispList $ head args of
                        Just ((LispSymbol name) : params) -> do
                            val <- lambda' $ toLispList params : tail args
                            setScope currScope { bindings = insert name val $ bindings currScope }
                        _ -> throwError $ BadSyntax "define function requires at least a name and a body"
                _ -> throwError $ BadSyntax "The name being defined must be a valid symbol"
        return LispVoid
    
    defineMacro args' = do
        currScope <- getScope
        args <- macroArgsFromLispList args'
        case args of
            (LispPair (LispSymbol name) (LispSymbol param)) : body -> do
                ident <- supplyIdent
                let val = LispMacro ident param body currScope
                setScope currScope { bindings = insert name val $ bindings currScope }
            _ -> throwError $ BadSyntax "define-macro requires a name, parameter, and body"
        return LispVoid
    
    add x = return $ case x of
        [LispRationalObject a, LispRationalObject b] -> LispRationalObject (             a + b)
        [LispRationalObject a, LispDoubleObject   b] -> LispDoubleObject   (fromRational a + b)
        [LispDoubleObject   a, LispRationalObject b] -> LispDoubleObject   (             a + fromRational b)
        [LispDoubleObject   a, LispDoubleObject   b] -> LispDoubleObject   (             a + b)

    subtract x = return $ case x of
        [LispRationalObject a, LispRationalObject b] -> LispRationalObject (             a - b)
        [LispRationalObject a, LispDoubleObject   b] -> LispDoubleObject   (fromRational a - b)
        [LispDoubleObject   a, LispRationalObject b] -> LispDoubleObject   (             a - fromRational b)
        [LispDoubleObject   a, LispDoubleObject   b] -> LispDoubleObject   (             a - b)

    multiply x = return $ case x of
        [LispRationalObject a, LispRationalObject b] -> LispRationalObject (             a * b)
        [LispRationalObject a, LispDoubleObject   b] -> LispDoubleObject   (fromRational a * b)
        [LispDoubleObject   a, LispRationalObject b] -> LispDoubleObject   (             a * fromRational b)
        [LispDoubleObject   a, LispDoubleObject   b] -> LispDoubleObject   (             a * b)

    divide x = case x of
        [numerator, LispRationalObject 0] -> throwError $ DivideByZero numerator
        [numerator, LispDoubleObject   0] -> throwError $ DivideByZero numerator
        [LispRationalObject a, LispRationalObject b] -> return $ LispRationalObject (             a / b)
        [LispRationalObject a, LispDoubleObject   b] -> return $ LispDoubleObject   (fromRational a / b)
        [LispDoubleObject   a, LispRationalObject b] -> return $ LispDoubleObject   (             a / fromRational b)
        [LispDoubleObject   a, LispDoubleObject   b] -> return $ LispDoubleObject   (             a / b)

    power x = return $ case x of
        [LispRationalObject a, LispRationalObject (b :% 1)] -> LispRationalObject (    a ^^ b)
        [LispRationalObject a, LispRationalObject b] -> LispDoubleObject (fromRational a ** fromRational b)
        [LispRationalObject a, LispDoubleObject   b] -> LispDoubleObject (fromRational a ** b)
        [LispDoubleObject   a, LispRationalObject b] -> LispDoubleObject (             a ** fromRational b)
        [LispDoubleObject   a, LispDoubleObject   b] -> LispDoubleObject (             a ** b)

    cons [first, second] = supplyIdent >>= \ident -> return $ LispPairObject ident first second
    car  [LispPairObject _ f _] = return f
    cdr  [LispPairObject _ _ s] = return s
    list = toLispObjectList

    quit [] = throwError $ RequestExit 0
    quit [LispRationalObject (code :% 1)] = throwError $ RequestExit (fromIntegral code)
    quit _ = throwError $ RequestExit 1