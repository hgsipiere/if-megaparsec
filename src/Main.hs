{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main where

import Prelude hiding (getLine)

import Control.Applicative hiding (many, some)
import Data.Text (Text)
import Data.Text.IO (getLine)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

x $> y = y <$ x

type Parser = Parsec Void Text

data Program = If' Program Program Program
             | Truth
             | Falsity deriving Show

evaluate Truth = True
evaluate Falsity = False
evaluate (If' a b c) = case evaluate a of
  True -> evaluate b
  False -> evaluate c

parseProg :: Parser Program
parseProg = lexeme $ parseTrue <|> parseFalse <|> parseIf
parseTrue = string "true" $> Truth
parseFalse = string "false" $> Falsity

lexeme = L.lexeme space
parens = between (char '(') (char ')')

parseIf = do 
  lexeme $ string "if"
  a <- lexeme $ parens parseProg
  b <- lexeme $ parens parseProg
  c <- lexeme $ parens parseProg
  return (If' a b c)

present (Left x) = errorBundlePretty x
present (Right x) = x

main :: IO ()
main = do
  line <- getLine
  putStrLn.present.(fmap $ show.evaluate) $ runParser parseProg "" line
