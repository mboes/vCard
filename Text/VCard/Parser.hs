module Text.VCard.Parser
    ( parseVCards, parseVCard ) where

import Text.VCard
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B


type SourceName = B.ByteString

parseVCards :: SourceName -> B.ByteString -> [VCard]
parseVCards file input =
    map parseVCard $ groupByCard $ D.parseDirectory (\x y -> [D.Text y]) input

groupByCard :: [Attribute] -> [[Attribute]]
groupByCard [] = []
groupByCard xs = tail $ foldr f [[]] xs
    where f p (ps:pss) | p D.@@ "begin" =
                           [] : (p:ps) : pss
          f p (ps:pss) = (p:ps):pss

parseVersion :: B.ByteString -> Version
parseVersion s = undefined

parseVCard :: [Attribute] -> VCard
parseVCard =
    foldr f VCard{ vcard_version = undefined, vcard_attributes = undefined }
    where f p vcard | p D.@@ "begin" = vcard -- administrative
                    | p D.@@ "end" = vcard   -- junk.
                    | p D.@@ "version" =
                        let D.Text ver = D.prop_value p
                        in vcard { vcard_version = parseVersion ver }
                    | otherwise = vcardMerge p vcard
