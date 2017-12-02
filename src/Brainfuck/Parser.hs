module Brainfuck.Parser (
    parse,
    ParseError(..),
    Command(..)
) where

import qualified Text.Parsec as Parsec (parse)
import           Text.Parsec hiding (parse)
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

statements :: Parsec String st [Command]
statements = catMaybes <$> many (try (Just <$> statement) <|> (anyOtherChar >> return Nothing))

statement :: Parsec String st Command
statement = command <|> loop

command :: Parsec String st Command
command = charToCommand <$> oneOf "+-.,<>"

loop :: Parsec String st Command
loop = Loop <$> between (char '[') (char ']') statements

anyOtherChar :: Parsec String st Char
anyOtherChar = noneOf "+-.,<>[]"

charToCommand :: Char -> Command
charToCommand '+' = Increment
charToCommand '-' = Decrement
charToCommand '.' = Output
charToCommand ',' = Input
charToCommand '<' = MoveLeft
charToCommand '>' = MoveRight
