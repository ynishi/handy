{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( module Lib
  , module Data.List
  , module Data.List.Split
  , module RIO
  , module RIO.FilePath
  ) where

import qualified Data.ByteString.Char8 as C8
import           Data.List
import           Data.List.Split
import           RIO
import qualified RIO.ByteString        as B
import           RIO.FilePath
import qualified RIO.Text              as T

inData file = joinPath ["data", file]

infile = inData "in.txt"

outfile = inData "out.txt"

data Quote
  = NonQ
  | Single String
  | Dif String
        String
  | Pre String
  | Suf String
  deriving (Eq, Show)

rep q d xs = intercalate (d ++ "\n") (map (deq q) xs)

rep' = rep sq ","

deq NonQ w       = w
deq (Single s) w = deq (Dif s s) w
deq (Pre s) w    = s ++ w
deq (Suf s) w    = w ++ s
deq (Dif s e) w  = deq (Pre s) . deq (Suf e) $ w

sq = Single "'"

mkLike t = mkOr . map (deq liq . deq sq)
  where
    liq = Pre (t ++ " like ")

mkOr = rep (Dif "(" ")") " or"

mkIn n t xs = mkOr (map (deq inq . rep') $ chunksOf n xs)
  where
    inq = Dif (t ++ " in (\n") "\n)"

mkIn1k = mkIn 1000

p :: MonadIO m => String -> m ()
p s = B.putStr . C8.pack $ s ++ "\n"

run f pre = do
  let infile = inData (pre ++ ".in.txt")
  p $ "infile:" ++ infile
  x <- words . C8.unpack <$> B.readFile infile
  p "read ok"
  let x' = f x
  p "ope ok"
  let outfile = inData (pre ++ ".out.txt")
  p $ "outfile:" ++ outfile
  B.writeFile outfile $ C8.pack x'
  p "write ok, check via below:"
  p $ "head " ++ outfile
  p $ "tail " ++ outfile
  p $ "wc -l " ++ outfile
