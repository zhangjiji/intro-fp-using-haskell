import System.IO
import System.Directory (getDirectoryContents, copyFile)
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket)

rmDuplicate [] = []
rmDuplicate (x:xs) = if x `elem` xs then rmDuplicate xs
           else x:(rmDuplicate xs)

rm :: String -> String
rm = foldr f ""
  where f x res
          | x `elem` res = res
          | otherwise = x:res

readContents :: FilePath -> IO String
readContents fn = bracket (openFile fn ReadMode) hClose
              (\h -> hGetContents h)

writeContents :: FilePath -> String -> IO ()
writeContents fn contents = bracket (openFile fn WriteMode) hClose
                        (\h -> hPutStrLn h contents)

main = do
  hr <- openFile "test.txt" ReadMode
  hSetEncoding hr utf8
  hw <- openFile "test2.txt" WriteMode
  hSetEncoding hw utf8

  contents <- hGetContents hr
  hPutStr hw (rm contents)

  hClose hw
  hClose hr
--  contents <- readContents "test.txt"
--  putStrLn (rm contents)
