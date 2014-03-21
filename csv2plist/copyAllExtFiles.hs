module CopyAllExtFiles (
  copyAllExtFiles
  )
where

import System.IO
import System.Directory (getDirectoryContents, copyFile)
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket)

copyAllExtFiles :: FilePath -> String -> FilePath -> IO ()
copyAllExtFiles sourceDir ext targetDir = do
  dirCon <- getDirectoryContents sourceDir
  let con = (filter ((== ext) . takeExtension) dirCon)
  mapM_ (\s -> copyFile s (targetDir </> s)) con
  
