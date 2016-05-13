{-# LANGUAGE GeneralizedNewtypeDeriving, MultiWayIf #-}

module Lexer where

import Data.Char
import Control.Monad.State
import Control.Monad.Except

data SourcePos = SourcePos 
               { srcLine :: !Int 
               , srcChar :: !Int
               }

instance Show SourcePos where
    show (SourcePos y x) = show y ++ ":" ++ show x

data LexErr = LexErr SourcePos LexErrType

instance Show LexErr where
    show (LexErr sp e) = show sp ++ "\t" ++ show e

data LexErrType = Unexpected Char
                | EndOfInput
                | WrongIndent
                deriving Eq

instance Show LexErrType where
    show e = case e of
        Unexpected c -> "unexpected " ++ [c]
        EndOfInput -> "unexpected end of input"

data Token = Token SourcePos TokType

instance Show Token where
    show (Token _ t) = show t

data TokType = NewlineTok
             | IndentTok
             | DedentTok
             | LParenTok
             | RParenTok
             | IntTok Integer
             | RealTok Double
             | IdentTok String
             deriving Eq

instance Show TokType where
    show t = case t of
        NewlineTok -> "end of file"
        IndentTok -> "indent"
        DedentTok -> "dedent"
        LParenTok -> "left parenthesis"
        RParenTok -> "right parenthesis"
        IntTok n -> "literal integer " ++ show n
        RealTok x -> "literal real " ++ show x
        IdentTok s -> "identifier " ++ s

data LexState = LexState
              { lexInput :: String
              , lexOutput :: [Token]
              , lexLine :: Int
              , lexChar :: Int
              , lexIndents :: [Int]
              } deriving Show

newtype Lexer a = Lexer (StateT LexState (Except LexErr) a)
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadState LexState
                         , MonadError LexErr
                         )

runLexer :: Lexer a -> String -> Either LexErr [Token]
runLexer (Lexer l) s = runExcept (reverse . lexOutput <$> execStateT l st)
  where st = LexState
           { lexInput = s
           , lexOutput = []
           , lexLine = 1
           , lexChar = 1
           , lexIndents = []
           }

getPos :: Lexer SourcePos
getPos = do
    l <- gets lexLine
    c <- gets lexChar
    return (SourcePos l c)

throwLexErr :: LexErrType -> Lexer a
throwLexErr e = do
    p <- getPos
    throwError (LexErr p e)

outputAt :: SourcePos -> TokType -> Lexer ()
outputAt p t = modify $ \st -> st { lexOutput = Token p t : lexOutput st }

output :: TokType -> Lexer ()
output t = do
    p <- getPos
    outputAt p t

indent :: Int -> Lexer ()
indent n = do
    output IndentTok
    modify $ \st -> st { lexIndents = n : lexIndents st }

dedent :: Lexer ()
dedent = do
    output DedentTok
    modify $ \st -> st { lexIndents = tail (lexIndents st) }

newline :: Lexer ()
newline = output NewlineTok

-- Very unsafe
peekChar :: Lexer Char
peekChar = do
    (x:_) <- gets lexInput
    return x 

skipChar :: Lexer ()
skipChar = modify $ \st -> st { lexInput = tail (lexInput st) }

skipWhile :: (Char -> Bool) -> Lexer ()
skipWhile cond = do
    eol <- endOfLine
    if eol then return () else do
        c <- peekChar
        if cond c 
          then skipChar *> skipWhile cond
          else return ()

endOfInput :: Lexer Bool
endOfInput = null <$> gets lexInput

endOfLine :: Lexer Bool
endOfLine = do
    eoi <- endOfInput
    if eoi then return True else (== '\n') <$> peekChar

lexTokens :: Lexer ()
lexTokens = do
    eoi <- endOfInput
    if eoi
      then do n <- length <$> gets lexIndents
              replicateM_ n dedent
      else lexToken *> lexTokens

isIdent :: Char -> Bool
isIdent = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ "_"))

isSpecial :: Char -> Bool
isSpecial = (`elem` "+-*/()[]")

lexToken :: Lexer ()
lexToken = do
    c <- peekChar
    if | c == '\n' -> do
         skipChar
         xs <- gets lexInput
         if all isSpace (takeWhile (/='\n') xs)
           then skipWhile isSpace *> lexToken 
           else lexIndent
       | isIdent c -> lexIdent
       | isDigit c -> lexNumber
       | isSpace c -> skipWhile isSpace
       | otherwise -> throwLexErr (Unexpected c)
     
lexIndent :: Lexer ()
lexIndent = do
    n <- countIndent
    xs <- gets lexIndents
    case xs of
        [] -> if n /= 0 then indent n else newline
        xs -> findIndent n xs

  where countIndent = do
            xs <- gets lexInput
            return (length (takeWhile isSpace xs))
        
        findIndent n (x:xs) = 
            case compare n x of
                EQ -> newline
                GT -> indent n
                _  -> do ds <- findDedents n xs
                         replicateM_ ds dedent
        
        findDedents n [] = return 1
        findDedents n (x:xs) =
            case compare n x of
                EQ -> return 1
                LT -> (+1) <$> findDedents n xs
                _  -> throwLexErr WrongIndent

collect :: (Char -> Bool) -> (Char -> Bool) -> Lexer String
collect contCond stopCond = do
    eol <- endOfLine
    if eol then return "" else do
        c <- peekChar 
        if | contCond c -> (c :) <$> (skipChar *> collect contCond stopCond)
           | stopCond c -> return ""
           | otherwise -> throwLexErr (Unexpected c)
         
lexIdent :: Lexer ()
lexIdent = do
    p <- getPos
    str <- collect (\c -> isIdent c || isDigit c)
                   (\c -> isSpecial c || isSpace c)
    outputAt p (IdentTok str)

lexNumber :: Lexer ()
lexNumber = do
    p <- getPos
    int <- collect isDigit (\c -> isSpecial c || isSpace c || c == '.')
    eol <- endOfLine
    if eol then outputAt p (IntTok (read int)) else do
        c <- peekChar
        if c == '.'
          then do skipChar
                  real <- collect isDigit (\c -> isSpecial c || isSpace c)
                  outputAt p (RealTok (read (int ++ "." ++ real)))
          else outputAt p (IntTok (read int))

