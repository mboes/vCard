module Text.VCard.Format.Directory
    ( readVCards, readVCard, writeVCard ) where

import Text.VCard
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B


type SourceName = String

readVCards :: SourceName -> B.ByteString -> [VCard]
readVCards file input =
    map readVCard $ groupByCard $ D.parseDirectory D.pa_text input

groupByCard :: [Attribute] -> [[Attribute]]
groupByCard [] = []
groupByCard xs = tail $ foldr f [[]] xs
    where f p (ps:pss) | p D.@@ "begin" =
                           [] : (p:ps) : pss
          f p (ps:pss) = (p:ps):pss

parseVersion :: B.ByteString -> Version
parseVersion s = let (majt, mint) = B.break (=='.') s
                     maj = maybe err fst (B.readInt majt)
                     min = maybe err fst (B.readInt (B.drop 1 mint))
                 in Version maj min
    where err = error "Not a valid version number."

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show

readVCard :: [Attribute] -> VCard
readVCard =
    foldr f VCard{ vcard_version = undefined, vcard_attributes = Map.empty }
    where f p vcard | p D.@@ "begin" = vcard -- administrative
                    | p D.@@ "end" = vcard   -- junk.
                    | p D.@@ "version" =
                        let D.Text ver = D.prop_value p
                        in vcard { vcard_version = parseVersion ver }
                    | otherwise = vcardMerge p vcard

writeVCard :: VCard -> B.ByteString
writeVCard (VCard ver attrs) =
    D.printDirectory $ [begin, version] ++ concat (Map.elems attrs) ++ [end]
    where attr typ val = D.Prop (D.nakedType typ) [] val
          begin   = D.Prop (D.nakedType "BEGIN") [] (D.Text "VCARD")
          end     = D.Prop (D.nakedType "END") [] (D.Text "VCARD")
          version = D.Prop (D.nakedType "VERSION") [] $
                    D.Text $ B.concat [ showBS $ version_major ver, "."
                                      , showBS $ version_minor ver ]
