{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Protolude
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Map as Map
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import           Data.Functor.Syntax ((<$$>))
import           System.FilePath (FilePath)
import qualified System.FilePath as FP


data PassFile = PassFile { pfName :: Text,
                           pfPath :: FilePath
                         } deriving (Show, Eq)

data PassDir = PassDir { pdName :: Text
                       , pdPath :: FilePath
                       , pdFiles :: [PassFile]
                       , pdChildren :: [PassDir]
                       } deriving (Show, Eq)


loadPass :: Text -> FilePath -> IO PassDir
loadPass name root = do
  entries <- (root </>) <$$> Dir.listDirectory root

  files <- filterM Dir.doesFileExist entries
  let gpgs = filter (\p -> Txt.takeEnd 4 (Txt.pack p) == ".gpg") files
  let passFiles = (\f -> PassFile (Txt.pack $ FP.takeBaseName f) f) <$> gpgs

  dirs <- filterM Dir.doesDirectoryExist entries
  passFolders' <- traverse (\p -> loadPass (Txt.pack $ FP.takeBaseName p) p) $ sort dirs
  let passFolders = filter isValidFolder passFolders'
  
  pure $ PassDir name root passFiles passFolders 

  where
    isValidFolder p = pdName p /= ".git" && length (pdChildren p) + length (pdFiles p) > 0

prnTree :: Bool -> PassDir -> Text
prnTree showFiles =
  go "" 

  where
    go pre p =
      let fs = if showFiles
               then Txt.concat $ (\f -> pre <> "    " <> pfName f <> "\n") <$> pdFiles p
               else "" in
    
      pre <> "  /" <> pdName p <> "\n" <> fs <> Txt.concat (go (pre <> "  ") <$> pdChildren p)
      
  
  
getDirs :: FilePath -> IO [FilePath]
getDirs p = do
  entries <- (p </>) <$$> Dir.listDirectory p
  filterM Dir.doesDirectoryExist entries

getFiles :: FilePath -> IO [FilePath]
getFiles p = do
  entries <- (p </>) <$$> Dir.listDirectory p
  filterM Dir.doesFileExist entries

getFilesRec :: FilePath -> IO [FilePath]
getFilesRec p = do
  fs <- (p </>) <$$> getFiles p
  ds <- (p </>) <$$> getDirs p
  cs <- traverse getFilesRec ds
  pure $ fs <> join cs
