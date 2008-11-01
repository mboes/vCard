module Main where

import qualified Data.ByteString.Lazy as B
import Text.VCard.Format.Directory
import System.IO
import System.Environment
import System.Exit

printUsage = putStrLn "vcard2vcard [FILE]" >> exitFailure

main = do
  args <- getArgs
  (filename, handle) <-
      case args of
        [file] -> openFile file ReadMode >>= return . ((,) file)
        [] -> return ("<stdin>", stdin)
        _ -> printUsage
  input <- B.hGetContents handle
  mapM_ (\v -> B.putStr (writeVCard v) >> B.putStr "\r\n") $ readVCards filename input

