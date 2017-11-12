{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Protolude

import qualified Lib
import qualified UI

main :: IO ()
main = do
  UI.main
  putText ""

{-
└── Tools
    ├── accuweather.com

│       ├ ─ ─  workopolis.com

│       ├ ─ ─ www.adp.com

└ ─ ─ 
-}
