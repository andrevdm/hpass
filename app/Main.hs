{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Protolude

import qualified BrickUi
import qualified CreateNew

main :: IO ()
main = --BrickUi.main
  CreateNew.main
