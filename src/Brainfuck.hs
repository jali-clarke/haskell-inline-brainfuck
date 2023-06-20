{-# LANGUAGE TemplateHaskell #-}

module Brainfuck (bf) where

import Brainfuck.Program (Program (..), interpret)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as ByteString
import Data.Functor (void, ($>))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH

bf :: TH.QuasiQuoter
bf =
  TH.QuasiQuoter
    { TH.quoteExp = bfQuoteExp,
      TH.quotePat = error "quotePat not implemented",
      TH.quoteType = error "quoteType not implemented",
      TH.quoteDec = error "quoteDec not implemented"
    }

bfQuoteExp :: String -> TH.Q TH.Exp
bfQuoteExp str =
  case Parser.parseOnly (brainfuckProgram <* Parser.endOfInput) (ByteString.pack str) of
    Left err -> fail err
    Right program -> fmap (TH.AppE (TH.VarE 'interpret)) (TH.liftData program)

singleInstruction :: Parser (Program -> Program)
singleInstruction =
  Parser.choice
    [ Parser.char '+' $> Inc,
      Parser.char '-' $> Dec,
      Parser.char '<' $> Lft,
      Parser.char '>' $> Rgt,
      Parser.char ',' $> Inp,
      Parser.char '.' $> Out
    ]

loopBody :: Parser (Program -> Program)
loopBody = do
  void $ Parser.char '['
  body <- brainfuckProgram
  void $ Parser.char ']'
  pure $ Loop body

skippedChar :: Parser (Program -> Program)
skippedChar = Parser.satisfy (not . (`elem` "+-<>,.[]")) $> id

brainfuckProgram :: Parser Program
brainfuckProgram = do
  instructions <- Parser.many' (singleInstruction <|> loopBody <|> skippedChar)
  pure $ foldr (.) id instructions Done
