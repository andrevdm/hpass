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


data Pass = PassFolder Text FilePath [Pass]
          | PassFile Text FilePath
          deriving (Show)


loadPass :: Text -> FilePath -> IO Pass
loadPass name root = do
  entries <- (root </>) <$$> Dir.listDirectory root

  files <- filterM Dir.doesFileExist entries
  let gpgs = filter (\p -> Txt.takeEnd 4 (Txt.pack p) == ".gpg") files
  let passFiles = (\f -> PassFile (Txt.pack $ FP.takeBaseName f) f) <$> gpgs

  dirs <- filterM Dir.doesDirectoryExist entries
  passFolders' <- traverse (\p -> loadPass (Txt.pack $ FP.takeBaseName p) p) $ sort dirs
  let passFolders = filter isValidFolder passFolders'
  
  pure $ PassFolder name root $ passFolders <> passFiles

  where
    isValidFolder (PassFolder n _ (_:_)) = n /= ".git"
    isValidFolder _ = False

prnTree :: Bool -> Pass -> Text
prnTree showFiles p =
  go "" p

  where
    go pre (PassFile name _) =
      if showFiles
        then pre <> "  " <> name <> "\n"
        else ""
    
    go pre (PassFolder name _ ps) =
      pre <> "  " <> name <> "\n" <> Txt.concat (go (pre <> "  ") <$> ps)
      
  
  
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
