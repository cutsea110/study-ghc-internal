{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where

import GHC.Prim
import GHC.Exts

cube :: Int -> Int
cube x = x `timesInt` x `timesInt` x

timesInt :: Int -> Int -> Int
{-# NOINLINE timesInt #-}
timesInt = \x y -> case x of
  I# x' -> case y of
    I# y' -> I# (x' *# y')
