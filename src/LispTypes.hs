{-# LANGUAGE DerivingVia, LambdaCase #-}

module LispTypes (
    LispObjectIdent (..),
    LispObject (..),
    toLispObject,
    fromLispObject,
    toLispObjectList,
    fromLispObjectList,
    LispScope (..),
    ScopeBindings,
    Environment (..),
    EnvironmentMonad (..),
    runEnvironment,
    RuntimeError (..)
) where

import GHC.Real
import Control.Monad
import Control.Monad.Supply
import Control.Monad.State
import Control.Monad.Except (runExceptT, ExceptT(..))
import qualified Control.Monad.Except as ExceptM (throwError, catchError)
import Data.List
import Control.Monad.Identity
import Data.HashMap.Strict (empty, HashMap)
import Control.Monad.Catch
import Parser

type Environment' = ExceptT RuntimeError (StateT LispScope (SupplyT LispObjectIdent IO))

newtype Environment a = Environment { extractEnv :: Environment' a }
    deriving (Monad, Applicative, Functor, MonadIO, MonadMask, MonadCatch, MonadThrow) via Environment'

runEnvironment :: Environment a -> Environment ScopeBindings -> IO ()
runEnvironment (Environment exc) (Environment bindings) = do
    let st = do
            result <- runExceptT $ do
                binds <- bindings
                put $ LispScope {parentScope = Nothing, bindings = binds}
                exc
            case result of
                Left err -> lift $ lift $ print err
                Right result -> return ()
        su = evalStateT st LispScope {parentScope = Nothing, bindings = empty}
        io = evalSupplyT su $ LispObjectIdent <$> [0..]
    io

class Monad m => EnvironmentMonad m where
    throwError :: RuntimeError -> m a
    catchError :: m a -> (RuntimeError -> m a) -> m a
    supplyIdent :: m LispObjectIdent
    getScope :: m LispScope
    setScope :: LispScope -> m ()
    runIn :: LispScope -> m a -> m a

instance EnvironmentMonad Environment where
    throwError = Environment . ExceptM.throwError
    catchError action handler = Environment $ ExceptM.catchError (extractEnv action) (extractEnv . handler)
    supplyIdent = Environment supply
    getScope = Environment get
    setScope = Environment . put
    runIn scope state = do
        oldScope <- getScope
        setScope scope
        result <- state
        setScope oldScope
        return result


newtype LispObjectIdent = LispObjectIdent Int
    deriving (Eq)

instance Show LispObjectIdent where
    show (LispObjectIdent x) = '@' : show x

data LispObject
    = LispVoid
    | LispNilObject
    | LispPairObject      LispObjectIdent LispObject LispObject
    | LispRationalObject                  Rational
    | LispDoubleObject                    Double
    | LispStringObject    LispObjectIdent String
    | LispSymbolObject                    String
    | LispFunction {
        ident           :: LispObjectIdent,
        fnParams        :: [String],
        fnVariadicParam :: Maybe String,
        fnBody          :: [LispExpr],
        fnScope         :: LispScope
    }
    | LispFunctionBuiltin LispObjectIdent ([LispObject] -> Environment LispObject)
    | LispMacro {
        ident      :: LispObjectIdent,
        macroParam :: String,
        macroBody  :: [LispExpr],
        macroScope :: LispScope
    }
    | LispMacroBuiltin    LispObjectIdent (LispExpr -> Environment LispObject)

toLispObject :: LispExpr -> Environment LispObject
toLispObject x = case x of
    LispNil -> return LispNilObject
    (LispPair a b)   -> do
        ident <- supplyIdent
        a' <- toLispObject a
        b' <- toLispObject b
        return $ LispPairObject ident a' b'
    (LispRational v) -> return $ LispRationalObject v
    (LispDouble v)   -> return $ LispDoubleObject v
    (LispString v)   -> do
        ident <- supplyIdent
        return $ LispStringObject ident v
    (LispSymbol v)   -> return $ LispSymbolObject v

fromLispObject :: LispObject -> Environment LispExpr
fromLispObject x = case x of
    LispNilObject            -> return LispNil
    (LispPairObject _ a' b') -> do
        a <- fromLispObject a'
        b <- fromLispObject b'
        return $ LispPair a b
    (LispRationalObject v)   -> return $ LispRational v
    (LispDoubleObject v)     -> return $ LispDouble v
    (LispStringObject _ v)   -> return $ LispString v
    (LispSymbolObject v)     -> return $ LispSymbol v
    _                        -> throwError $ NotExpandable x


toLispObjectList :: [LispObject] -> Environment LispObject
toLispObjectList = \case
    x:xs -> do
        ident <- supplyIdent
        rest <- toLispObjectList xs
        return $ LispPairObject ident x rest
    _ -> return LispNilObject 

fromLispObjectList :: LispObject -> Maybe [LispObject]
fromLispObjectList LispNilObject = return []
fromLispObjectList (LispPairObject _ a xs) = fromLispObjectList xs >>= \rest -> return $ a : rest
fromLispObjectList _ = Nothing


instance Eq LispObject where
    LispNilObject              == LispNilObject              = True
    (LispPairObject i1 _ _)    == (LispPairObject i2 _ _)    = i1 == i2
    (LispRationalObject a)     == (LispRationalObject b)     = a  == b
    (LispDoubleObject a)       == (LispDoubleObject b)       = a  == b
    (LispStringObject i1 _)    == (LispStringObject i2 _)    = i1 == i2
    (LispSymbolObject a)       == (LispSymbolObject b)       = a  == b
    (LispFunction i1 _ _ _ _)  == (LispFunction i2 _ _ _ _)  = i1 == i2
    (LispFunctionBuiltin i1 _) == (LispFunctionBuiltin i2 _) = i1 == i2
    (LispMacro i1 _ _ _)       == (LispMacro i2 _ _ _)       = i1 == i2
    (LispMacroBuiltin i1 _)    == (LispMacroBuiltin i2 _)    = i1 == i2
    _                          == _                          = False


instance Show LispObject where
    show x
        | LispNilObject      <- x = '\'' : show' x
        | LispPairObject {}  <- x = '\'' : show' x
        | LispSymbolObject _ <- x = '\'' : show' x
        | otherwise               =        show' x
        where
            show' LispVoid = "<#void>"
            show' LispNilObject = "()"
            show' (LispPairObject _ (LispSymbolObject "quote") (LispPairObject _ q LispNilObject)) = '\'' : show' q
            show' (LispPairObject _ q qs) = '(' : show' q ++ showPairChain qs ++ ")" where
                showPairChain LispNilObject          = ""
                showPairChain (LispPairObject _ a b) = ' ' : show' a ++ showPairChain b
                showPairChain x                      = " . " ++ show' x

            show' (LispRationalObject (a :% 1))         = show a
            show' (LispRationalObject (a :% b))         = show a ++ '/' : show b
            show' (LispDoubleObject v)                  = show v
            show' (LispStringObject _ s)                = '"' : s ++ "\"" -- Update to print out escape sequences
            show' (LispSymbolObject x)                  = x
            show' (LispFunction _ args varg body scope) = case varg of
                Just v ->  "(lambda (" ++ unwords args ++ " . " ++ v ++ ") " ++ unwords (show <$> body) ++ ")" 
                Nothing -> "(lambda (" ++ unwords args ++               ") " ++ unwords (show <$> body) ++ ")" 
            show' (LispFunctionBuiltin id _)            = "<#lambda " ++ show id ++ ">" 
            show' (LispMacro _ arg body scope)          = "(macro (" ++ arg ++ ") " ++ unwords (show <$> body) ++ ")" 
            show' (LispMacroBuiltin id _)               = "<#macro " ++ show id ++ ">" 


type ScopeBindings = HashMap String LispObject

data LispScope = LispScope {
    parentScope :: Maybe LispScope,
    bindings    :: ScopeBindings
}

data RuntimeError
    = RequestExit Int -- {exitCode}
    | InvokingEmptyList
    | NotInvokable LispObject
    | NotExpandable LispObject
    | NotBoundInScope String
    | WrongArgumentCount Int Int -- {expected, found}
    | ExpectViolation String LispObject -- {expected, found}
    | BadSyntax String
    | DivideByZero LispObject
    | RuntimeError String String -- {type, message} For user-defined errors, capable of emulating built-in errors
    | HaskellError SomeException 
    deriving (Show)

class LispError a where
    errorType :: a -> String
    errorMessage :: a -> String

instance LispError RuntimeError where
    errorType InvokingEmptyList         = "InvokingEmptyList"
    errorType (NotInvokable _         ) = "NotInvokable"
    errorType (NotExpandable _        ) = "NotExpandable"
    errorType (NotBoundInScope _      ) = "NotBoundInScope"
    errorType (WrongArgumentCount _ _ ) = "WrongArgumentCount"
    errorType (ExpectViolation _ _    ) = "ExpectViolation"
    errorType (BadSyntax _            ) = "BadSyntax"
    errorType (DivideByZero _         ) = "DivideByZero"
    errorType (RuntimeError t _       ) = t
    errorMessage (RuntimeError _ msg) = msg
    errorMessage _ = ""