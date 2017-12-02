module Brainfuck.Util (
      matchBounds,
      replaceAtNth,
      getWord8,
      putWord8
) where

import Data.Word (Word8)
import Data.Char (ord, chr)

matchBounds :: [a] -> a -> Int -> [a]
matchBounds as def pos | outOfBounds = as ++ replicate overflow def
                       | otherwise   = as
    where overflow = pos + 1 - length as
          outOfBounds = overflow > 0

replaceAtNth :: [a] -> Int -> a -> [a]
replaceAtNth as pos r = let (pre, (_:res)) = splitAt pos as
                        in pre ++ (r:res)

charToWord8 :: Char -> Word8
charToWord8 c = fromIntegral (ord c) :: Word8

word8ToChar :: Word8 -> Char
word8ToChar w = chr (fromIntegral w :: Int)

getWord8 :: IO Word8
getWord8 = charToWord8 <$> getChar

putWord8 :: Word8 -> IO ()
putWord8 = putChar . word8ToChar
