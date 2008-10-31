module Text.VCard.Format.Directory
    ( readVCards, fromAttributes, writeVCard ) where

import Text.VCard
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B


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
