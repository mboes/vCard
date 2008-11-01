module Text.VCard.Format.Directory
    ( readVCards, fromAttributes, writeVCard ) where

import Text.VCard
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Codec.Binary.Base64.String as Base64
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Printf
import Data.List (intercalate)


instance D.PrintValue ExtraValue where
    printValue (Sequence xs) =
        D.escape ",;" $ B.intercalate "," $ intercalate [";"] xs
    printValue (Binary blob) = B.pack $ Base64.encode $ B.unpack blob
    printValue (PhoneNumber num) = num
    printValue (UTCOffset sign hrs mins) =
        B.pack $ printf "%c%02d:%02d" sign hrs mins
    printValue (SubVCard vc) = D.escape ",;:" $ writeVCard vc

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show

writeVCard :: VCard -> B.ByteString
writeVCard (VCard ver attrs) =
    D.printDirectory $ [begin, version] ++ concat (Map.elems attrs) ++ [end]
    where attr typ val = D.Prop (D.nakedType typ) [] val
          begin   = D.Prop (D.nakedType "BEGIN") [] (D.Text "VCARD")
          end     = D.Prop (D.nakedType "END") [] (D.Text "VCARD")
          version = D.Prop (D.nakedType "VERSION") [] $
                    D.Text $ B.concat [ showBS $ version_major ver, "."
                                      , showBS $ version_minor ver ]

readVCards :: SourceName -> B.ByteString -> [VCard]
readVCards file =
    map fromAttributes . D.groupByBeginEnd . D.parseDirectory' D.pa_text

parseVersion :: B.ByteString -> Version
parseVersion s = let (majt, mint) = B.break (=='.') s
                     maj = maybe err fst (B.readInt majt)
                     min = maybe err fst (B.readInt (B.drop 1 mint))
                 in Version maj min
    where err = error "Not a valid version number."

fromAttributes :: [Attribute] -> VCard
fromAttributes =
    foldr f VCard{ vcard_version = undefined, vcard_attributes = Map.empty }
    where f p vcard | p D.@@ "begin" = vcard -- administrative
                    | p D.@@ "end" = vcard   -- junk.
                    | p D.@@ "version" =
                        let D.Text ver = D.prop_value p
                        in vcard { vcard_version = parseVersion ver }
                    | otherwise = vcardMerge p vcard

fields :: B.ByteString -> [B.ByteString]
fields "" = []
fields s = B.foldr f [B.empty] s
    where f ';'  (xs:xss) = B.empty : xs : xss
          f '\\' ("":xs:xss) = B.cons ';' xs : xss
          f '\\' (xs:xss) | Just ('\\',_) <- B.uncons xs = B.cons '\\' xs : xss
          f x (xs:xss) = B.cons x xs : xss

-- | A variant of RFC 2425 text type where all ';' characters are escaped
-- except those that serve as field delimiters.
pa_sequence :: D.ValueParser ExtraValue
pa_sequence tps =
    return . D.IANAValue . Sequence . map (map untxt . D.pa_text tps) . fields
    where untxt (D.Text s) = s

pa_binary :: D.ValueParser ExtraValue
pa_binary _ = return . D.IANAValue . Binary . B.pack . Base64.decode . B.unpack
