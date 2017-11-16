{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto where

import           Protolude
import qualified Data.Text as Txt
import qualified Crypto.Random as R
import qualified Data.ByteString as BS

newtype Seed = Seed R.ChaChaDRG
data PasswordOptions = PasswordOptins { poUseLower :: Bool
                                      , poUseCaps :: Bool
                                      , poUseNum :: Bool
                                      , poUseSymbol :: Bool
                                      , poRemoveAmbig :: Bool
                                      , poLength :: Int
                                      }
  
genSeed :: IO Seed
genSeed = do
  s <- R.seedNew
  let c = R.drgNewSeed s
  pure $ Seed c
  
  
genPassword :: PasswordOptions -> Seed -> (Text, Seed)
genPassword opts (Seed c) = 
  let len = max 1 (min 256 (poLength opts)) in
  
  let (r :: BS.ByteString, next) = R.randomBytesGenerate len c  in
  let cs1 = (if poUseLower opts then ['a'..'z'] else [])
         <> (if poUseCaps opts then ['A'..'Z'] else [])
         <> (if poUseNum opts then ['0'..'9'] else [])
         <> (if poUseSymbol opts then "~!@#$%^&*()-_+;'\\|/" else []) in

  let cs2 = if poRemoveAmbig opts
           then Txt.unpack $ Txt.replace "l:O" "" $ Txt.pack cs1
           else cs1 in

  let cs = if null cs2 then "?" else cs2 in
    
  let p' = BS.foldl' (\a b -> a <> [cs `atMay` (fromIntegral b `mod` length cs)]) [] r in
  let p = fromMaybe "" $ sequenceA p' in
  (Txt.pack p, Seed next)
