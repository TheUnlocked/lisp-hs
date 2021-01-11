{-# LANGUAGE LambdaCase #-}

module Parser (
    LispExpr (..),
    toLispList,
    fromLispList,
    fromLispList',
    ParseError (..),
    LispToken (..),
    ParseSource (..),
    LispTokenType (..),
    readLispWithTokens,
    readLisp,
    readTokens,
    isBreaking
) where

import Numeric
import GHC.Real
import Text.Read ( readMaybe )
import Control.Monad.State
import Data.Char
import Data.HashMap.Strict (fromList, HashMap, (!?))
import qualified Data.List.Safe as Safe
import Util.List
import Control.Monad.Writer
import Control.Monad.Except (runExceptT, ExceptT)
import qualified Control.Monad.Except as ExceptM (throwError, catchError)


data LispExpr
    = LispPair LispExpr LispExpr
    | LispNil
    | LispRational Rational
    | LispDouble Double
    | LispSymbol String
    | LispString String
    deriving (Eq)

toLispList :: [LispExpr] -> LispExpr
toLispList = foldr LispPair LispNil

fromLispList :: LispExpr -> Maybe [LispExpr]
fromLispList LispNil = return []
fromLispList (LispPair a xs) = fromLispList xs >>= \rest -> return $ a : rest
fromLispList _ = Nothing

fromLispList' :: LispExpr -> ([LispExpr], Maybe LispExpr)
fromLispList' LispNil = ([], Nothing)
fromLispList' (LispPair a xs) = let (rest, last) = fromLispList' xs in (a : rest, last)
fromLispList' x = ([], Just x)


showLisp :: LispExpr -> String
showLisp (LispSymbol x) = x
showLisp (LispRational (a :% 1)) = show a
showLisp (LispRational (a :% b)) = show a ++ '/' : show b
showLisp (LispDouble v) = show v
showLisp (LispPair (LispSymbol "quote") (LispPair q LispNil)) = '\'' : show q
showLisp (LispPair q qs) = '(' : show q ++ showPairChain qs ++ ")"
    where showPairChain LispNil = ""
          showPairChain (LispPair a b) = ' ' : show a ++ showPairChain b
          showPairChain x                            = " . " ++ show x
showLisp (LispString s) = '"' : s ++ "\"" -- Update to print out escape sequences

instance Show LispExpr where
    show = showLisp


data ParseSource = ParseSource { row :: Int, col :: Int }
    deriving (Show)

data LispTokenType
    = TokenParenL
    | TokenParenR
    | TokenQuote
    | TokenSymbol String
    | TokenRational String Rational
    | TokenDouble String Double
    | TokenString String String
    | TokenDot
    | TokenComment String
    | TokenWhitespace String
    | TokenEOL
    | TokenEOF
    deriving (Show)

data LispToken = LispToken LispTokenType ParseSource
    deriving (Show)

data ParseError
    = NoInput -- A special case which is not actually an error, but behaves as one for parsing
    | StrandedQuote
    | MissingClosingParen
    | ExtraClosingParen
    | DivideByZeroFraction Integer
    | MissingEndQuote
    | InvalidEscapeSequence String
    | InvalidPairDot
    deriving (Show, Eq)


data ParseState = ParseState { remainingInput :: String, cursor :: ParseSource }

startParseState :: String -> ParseState
startParseState str = ParseState { remainingInput = str, cursor = ParseSource { row = 0, col = 0 } }


type Parser a = ExceptT ParseError (WriterT [LispToken] (State ParseState)) a

runParser :: Parser a -> ParseState -> (Either ParseError a, [LispToken], ParseState)
runParser p st =
    let e = runExceptT p
        w = runWriterT e
        s = runState w st
        ((t1, t2), t3) = s
    in (t1, t2, t3)

throwError :: ParseError -> Parser a
throwError = ExceptM.throwError

catchError :: Parser a -> (ParseError -> Parser a) -> Parser a
catchError = ExceptM.catchError

getCursor :: Parser ParseSource
getCursor = do
    ParseState {cursor = cursor} <- get
    return cursor

putToken :: LispTokenType -> Parser ()
putToken t = do
    cursor <- getCursor
    tell [LispToken t cursor]

putToken' :: LispToken -> Parser ()
putToken' t = tell [t]



isBreaking :: Char -> Bool
isBreaking = liftM2 (||) isSpace (`elem` ['(', ')', ';'])


readInlineWhitespace :: String -> Parser ()
readInlineWhitespace str = do
    let (ws, rest) = span (liftM2 (&&) isSpace (/= '\n')) str
    putToken $ TokenWhitespace ws
    cursor <- getCursor
    put $ ParseState rest cursor { col = col cursor + length ws }

readLispExpr :: Parser LispExpr
readLispExpr = do
    ParseState { remainingInput = str, cursor = source } <- get
    case str of
        ""          -> do
            putToken TokenEOF
            throwError NoInput
        '\n' : rest -> do
            putToken TokenEOL
            put $ ParseState rest source { row = row source + 1, col = 0 }
            readLispExpr
        c : rest | isSpace c -> do
            readInlineWhitespace $ c : rest
            readLispExpr
        ';'  : rest -> do
            let (comment, afterComment) = break (== '\n') rest
            putToken $ TokenComment (';' : comment)
            put $ ParseState afterComment source { col = col source + 1 + length comment }
            readLispExpr
        ')'  : rest -> do
            putToken TokenParenR
            put $ ParseState rest source { col = col source + 1 }
            throwError ExtraClosingParen
        '"'  : rest -> do
            put $ ParseState rest source { col = col source + 1 }
            s <- readLispString
            ParseState { remainingInput = rest'' } <- get
            putToken' $ LispToken (TokenString ('"' : take (length rest - length rest'') rest) s) source
            return $ LispString s
        '\'' : rest -> do
            putToken TokenQuote
            put $ ParseState rest source { col = col source + 1 }
            x <- catchError readLispExpr $ const $ do
                put $ ParseState rest source { col = col source + 1 }
                throwError StrandedQuote
            return (toLispList [LispSymbol "quote", x])
        '('  : rest -> do
            putToken TokenParenL
            put $ ParseState rest source { col = col source + 1 }
            readLispList
        _           -> do
            let (v, rest) = break isBreaking str
            put $ ParseState rest source { col = col source + length v }
            if v == "." then
                throwError InvalidPairDot
            else if any (`elem` ['e', 'E', '.']) v then
                -- Check for floating-point number
                case readSigned readFloat v of
                    [(num, "")] -> do
                        putToken' $ LispToken (TokenDouble v num) source
                        return $ LispDouble num
                    _           -> do
                        putToken' $ LispToken (TokenSymbol v) source
                        return $ LispSymbol v
            else
                -- Check for integer
                case readSigned readDec v of
                    [(num, "")] -> do
                        putToken' $ LispToken (TokenRational v (num % 1)) source
                        return (LispRational $ num % 1)
                    _ ->
                        -- Check for fraction notation
                        let (f, s) = break (== '/') v
                            rat = do
                                a <- readMaybe f
                                b <- readMaybe =<< Safe.tail s
                                return (a, b)
                        in case rat of
                            Just (a, b) ->
                                -- Dividing by zero is a syntax error
                                if b == 0 then
                                    throwError (DivideByZeroFraction a)
                                else do
                                    putToken' $ LispToken (TokenRational v (a % b)) source
                                    return $ LispRational (a % b)
                            _ -> do
                                putToken' $ LispToken (TokenSymbol v) source
                                return $ LispSymbol v


readLispList :: Parser LispExpr
readLispList = do
    ParseState { remainingInput = str, cursor = source } <- get
    case str of
        ""                              ->
            throwError MissingClosingParen
        -- Whitespace handling is necessary because trailing whitespace isn't handled by readLispExpr
        '\n' : rest                     -> do
            putToken TokenEOL
            put $ ParseState rest source { row = row source + 1, col = 0 }
            readLispList
        c    : rest | isSpace c         -> do
            readInlineWhitespace $ c : rest
            readLispList
        ('.' : rest)                    -> do
            putToken TokenDot
            put $ ParseState rest source { col = col source + 1 }
            v <- catchError readLispExpr $ \case
                ExtraClosingParen -> throwError InvalidPairDot
                NoInput           -> throwError InvalidPairDot
                err               -> throwError err
            let findRightParen = do
                    ParseState { remainingInput = str, cursor = source } <- get
                    case str of
                        '\n' : rest             -> do
                            putToken TokenEOL
                            put $ ParseState rest source { row = row source + 1, col = 0 }
                            findRightParen
                        c    : rest | isSpace c -> do
                            readInlineWhitespace $ c : rest
                            findRightParen
                        ')'  : rest             -> do
                            putToken TokenParenR
                            put $ ParseState rest source { col = col source + 1 }
                            return v
                        _ -> throwError MissingClosingParen
            findRightParen

        (')' : rest)                    -> do
            putToken TokenParenR
            put $ ParseState rest source { col = col source + 1 }
            return LispNil
        _                               -> do
            next <- readLispExpr
            LispPair next <$> readLispList


readLispString :: Parser String
readLispString = do
    ParseState { remainingInput = str, cursor = source } <- get
    case str of
        ""              ->
            throwError MissingEndQuote
        '\\' : rest -> do
            put $ ParseState rest source { col = col source + 1 }
            char <- readEscapeSequence
            restOfString <- readLispString
            return $ char : restOfString
        '"' : rest      -> do
            put $ ParseState rest source { col = col source + 1 }
            return ""
        c   : rest      -> do
            if c == '\n' then
                put $ ParseState rest source { row = row source + 1, col = 0 }
            else
                put $ ParseState rest source { col = col source + 1 }
            restOfString <- readLispString
            return $ c : restOfString

escapeSequences :: HashMap Char Char
escapeSequences = fromList [
        ('a', '\a'),
        ('b', '\b'),
        ('t', '\t'),
        ('n', '\n'),
        ('\n', '\n'),
        ('v', '\v'),
        ('f', '\f'),
        ('r', '\r'),
        ('e', '\ESC'),
        ('"', '"'),
        ('\'', '\''),
        ('\\', '\\')
    ]

readEscapeSequence :: Parser Char
readEscapeSequence = do
    ParseState { remainingInput = str, cursor = source } <- get
    let returnChr t "" = throwError (InvalidEscapeSequence [t])
        returnChr t seq = 
            let ordinal = fst . head $ readHex seq in
            if ordinal < 0 || ordinal > ord maxBound then
                throwError (InvalidEscapeSequence $ t : seq)
            else
                return (chr ordinal)

    case str of
        "" ->
            throwError (InvalidEscapeSequence "")
        c : rest | c `elem` ['x', 'u', 'U'] -> do
            -- TODO: Handle UTF-16 surrogate pairs for \u
            let maxSize = case c of
                    'x' -> 2
                    'u' -> 4
                    'U' -> 8
            let (seq, afterSeq) = spanLimited isHexDigit maxSize rest
            put $ ParseState afterSeq source { col = col source + 1 + length seq }
            returnChr c seq
        c : rest -> do
            let found = escapeSequences !? c
            case found of
                Just found -> do
                    if c == '\n' then
                        put $ ParseState rest source { row = row source + 1, col = 0 }
                    else
                        put $ ParseState rest source { col = col source + 1 }
                    return found
                Nothing -> do
                    let (seq, rest) = spanLimited isOctDigit 3 (c : rest)
                    put $ ParseState rest source { col = col source + length seq }
                    if seq == "" then
                        throwError (InvalidEscapeSequence "")
                    else do
                        let ordinal = fst . head $ readOct seq
                        if ordinal >= 0 && ordinal < 256 then
                            return (chr ordinal)
                        else
                            throwError (InvalidEscapeSequence seq)


readTokens :: String -> [LispToken]
readTokens = fst . readLispWithTokens

readLisp :: String -> Either [ParseError] [LispExpr]
readLisp = snd . readLispWithTokens

readLispWithTokens :: String -> ([LispToken], Either [ParseError] [LispExpr])
readLispWithTokens str = do
    let w1 = runWriterT (getValues $ startParseState str)
        ((exprs, tokens), errors) = runWriter w1
    if null errors then
        (tokens, Right exprs)
    else 
        (tokens, Left errors)
    where
    getValues :: ParseState -> WriterT [LispToken] (Writer [ParseError]) [LispExpr]
    getValues s =
        case s of
            ParseState { remainingInput = "" } -> return [] 
            st -> do
                let (nextResult, tokens, nextState) = runParser readLispExpr st
                tell tokens
                case nextResult of
                    (Left  NoInput) ->
                        getValues nextState
                    (Left  nextErr) -> do
                        lift $ tell [nextErr]
                        getValues nextState
                    (Right nextVal) -> do
                        nextVals <- getValues nextState
                        return $ nextVal : nextVals


instance Read LispExpr where
    readsPrec _ str =
        let (result, tokens, finalState) = runParser readLispExpr (startParseState str) in
            case result of
                (Left _)   -> []
                (Right x)  -> [(x, remainingInput finalState)]
