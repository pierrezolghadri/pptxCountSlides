{-# LANGUAGE OverloadedStrings #-}

import           Codec.Archive.Zip    (filesInArchive, toArchive)
import qualified Data.ByteString.Lazy as B
import           Data.List            (isInfixOf, isSuffixOf)
import           System.Directory

returnDirectoryContents :: IO [FilePath]
returnDirectoryContents = do
  filter (isSuffixOf "pptx") <$> getDirectoryContents "."

main :: IO ()
main = do
  p <- returnDirectoryContents
  e <- map pp <$> mapM countSlides p
  writeFile "list.txt" (unlines e)

pp :: Show a => (a, [Char]) -> [Char]
pp (a, b) = concat [b, ": ", show a, " slides"]

countSlides :: FilePath -> IO (Int, FilePath)
countSlides path = do
  r <- B.readFile path
  let result =
        filter ((&&) <$> isInfixOf "ppt/slides" <*> isSuffixOf "xml") $
        filesInArchive (toArchive r)
  return $ length result `seq` (length result, path)
