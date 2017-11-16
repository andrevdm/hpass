{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Protolude

import qualified BrickUi
import qualified CreateNew

main :: IO ()
main = do --BrickUi.main
  r <- CreateNew.runCreatePassword
  print r
