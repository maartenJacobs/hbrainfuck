module Brainfuck.Interpreter (
    evalCommands,
    initStack,
    Stack(..)
) where

import Brainfuck.Parser
import Brainfuck.Util
import Data.Word        (Word8)
import Prelude          hiding (read, print)

type Pos = Int
data Stack = Stack [Word8] Pos
    deriving (Show)

initStack :: Stack
initStack = Stack [] 0

getValue :: Stack -> Word8
getValue (Stack bs pos) | outOfBounds = 0
                        | otherwise   = bs !! pos
    where outOfBounds = pos > length bs - 1

modifyValue :: (Word8 -> Word8) -> Stack -> Stack
modifyValue mod st@(Stack bs pos) = Stack (replaceAtNth (matchBounds bs 0 pos) pos (mod $ getValue st)) pos

moveLeft :: Stack -> Stack
moveLeft st@(Stack bs pos) | pos == 0 = st
                           | pos > 0  = Stack bs (pos - 1)

moveRight :: Stack -> Stack
moveRight (Stack bs pos) = Stack bs (pos + 1)

increment :: Stack -> Stack
increment = modifyValue (+ 1)

decrement :: Stack -> Stack
decrement = modifyValue (subtract 1)

print :: Stack -> IO ()
print = putWord8 . getValue

read :: Stack -> IO Stack
read st = do w <- getWord8
             return $ modifyValue (const w) st

eval :: Command -> Stack -> IO Stack
eval MoveRight   st = return $ moveRight st
eval MoveLeft    st = return $ moveLeft st
eval Increment   st = return $ increment st
eval Decrement   st = return $ decrement st
eval Output      st = print st >> return st
eval Input       st = read st
eval (Loop cmds) st = loop (\st -> getValue st /= 0) cmds st

loop :: (Stack -> Bool) -> [Command] -> Stack -> IO Stack
loop cond cmds st = if cond st
                        then do st' <- evalCommands cmds st
                                loop cond cmds st'
                        else return st

evalCommands :: [Command] -> Stack -> IO Stack
evalCommands [] st       = return st
evalCommands (c:cmds) st = do st' <- eval c st
                              evalCommands cmds st'
