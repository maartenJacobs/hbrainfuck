module Brainfuck.Parser (
    parse,
    ParseError(..),
    Command(..)
) where

import qualified Text.Parsec as Parsec (parse)
import           Text.Parsec hiding (parse)
import           Text.Parsec.String (Parser)
import           Data.Maybe  (catMaybes)

{-
statements  -> statement*
statement   -> command | [ statements? ] | anyCharacter
command     -> + | - | < | > | . | ,
-}

parse :: String -> Either ParseError [Command]
parse code = Parsec.parse statements code code

data Command = MoveRight | MoveLeft
             | Increment | Decrement
             | Output    | Input
             | Loop [Command]
            deriving (Show, Eq)

statements :: Parser [Command]
statements = catMaybes <$> many (try (Just <$> statement) <|> (anyOtherChar >> return Nothing))

statement :: Parser Command
statement = command <|> loop

command :: Parser Command
command = charToCommand <$> oneOf "+-.,<>"

loop :: Parser Command
loop = Loop <$> between (char '[') (char ']') statements

anyOtherChar :: Parser Char
anyOtherChar = noneOf "+-.,<>[]"

charToCommand :: Char -> Command
charToCommand '+' = Increment
charToCommand '-' = Decrement
charToCommand '.' = Output
charToCommand ',' = Input
charToCommand '<' = MoveLeft
charToCommand '>' = MoveRight
