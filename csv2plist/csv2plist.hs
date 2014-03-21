import Text.ParserCombinators.Parsec
import CopyAllExtFiles

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = do
  char '"'
  content <- many quotedChar
  char '"' <?> "quote at end of cell"
  return content

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> try (string "\n")
      <|> try (string "\r")
      <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

{-
copyFile :: FilePath -> FilePath -> IO ()
copyFile source target = do
  contents <- readContents source
  writeContents target contents

readContents :: FilePath -> IO String
readContents fn = bracket (openFile fn ReadMode) hClose
              (\h -> hGetContents h)

writeContents :: FilePath -> String -> IO ()
writeContents fn contents = bracket (openFile fn WriteMode) hClose
                        (\h -> hPutStrLn h contents)
-}

main = do
  c <- getContents
  case parse csvFile "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ print r
