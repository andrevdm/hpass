{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Protolude
import qualified Data.Text as Txt
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import           Data.Functor.Syntax ((<$$>))
import qualified System.FilePath as FP


data PassFile = PassFile { pfName :: Text
                         , pfPath :: FilePath
                         , pfPassPath :: Text
                         } deriving (Show, Eq)

data PassDir = PassDir { pdName :: Text
                       , pdPath :: FilePath
                       , pdFiles :: [PassFile]
                       , pdChildren :: [PassDir]
                       , pdDepth :: Int
                       } deriving (Show, Eq)


loadPass :: Int -> Text -> FilePath -> IO PassDir
loadPass depth' name' root = 
  go depth' name' root

  where
    go depth name atPath = do
      entries <- (atPath </>) <$$> Dir.listDirectory atPath

      files <- filterM Dir.doesFileExist entries
      let gpgs = filter (\p -> Txt.takeEnd 4 (Txt.pack p) == ".gpg") files
      let passFiles = mkFile <$> sort gpgs

      dirs <- filterM Dir.doesDirectoryExist entries
      passFolders' <- traverse (\p -> go (depth + 1) (Txt.pack $ FP.takeBaseName p) p) $ sort dirs
      let passFolders = filter isValidFolder passFolders'
      
      pure $ PassDir name atPath passFiles passFolders depth

    isValidFolder p = pdName p /= ".git" && length (pdChildren p) + length (pdFiles p) > 0

    mkFile gpgPath =
      let p = Txt.pack gpgPath in
      let p1 = Txt.drop (length root + 1) p in
        
      PassFile { pfName = Txt.pack $ FP.takeBaseName gpgPath
               , pfPath = gpgPath
               , pfPassPath = p1
               }

flattenDirs :: PassDir -> [PassDir]
flattenDirs p =
  [p] <> (flattenDirs =<< pdChildren p)

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
