import Text.VCard.Format.Directory

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Char8.Caseless as I
import Text.VCard.Format.Directory
import qualified Text.VCard.Query as Q
import Text.Regex.PCRE.ByteString.Lazy
import Control.Monad (when)
import System.IO
import System.IO.Unsafe
import System.Environment
import System.Exit

printUsage = putStrLn "mutt_vcard_query FILE PATTERN" >> exitFailure

(=~) :: Regex -> B.ByteString -> Bool
r =~ str = unsafePerformIO $
     execute r str >>= return . either (error . snd) (maybe False (const True))

mkLine :: [I.ByteString] -> VCard -> B.ByteString
mkLine spec card =
    B.intercalate "\t" $ map elim $ map (flip Q.lookup card) spec
    where elim (Just [Text x]) = x
          elim _ = ""

main = do
  args <- getArgs
  (filename, restr, handle)
      <- case args of
           [file,restr] -> openFile file ReadMode >>=
                           return . ((,,) file (B.pack restr))
           _ -> printUsage
  dirstr <- B.hGetContents handle
  result <- compile compCaseless execBlank restr
  let re = case result of Left (_, err) -> error err
                          Right re -> re
      cards = readVCards filename dirstr
      matches = Q.filterWithAttribute query cards
      query prop | prop @@ "fn", Text name <- prop_value prop,
                   re =~ name = True
                 | otherwise = False
  when (null matches) $ putStrLn "No matches found." >> exitFailure
  putStrLn $ "Entries: " ++ show (length cards) ++ " Matching: " ++ show (length matches)
  let lines = map (mkLine ["email", "fn", "note"]) matches
  mapM_ B.putStrLn lines
  return exitSuccess
