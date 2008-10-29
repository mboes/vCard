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

readVCard :: [Attribute] -> VCard
readVCard =
    foldr f VCard{ vcard_version = undefined, vcard_attributes = Map.empty }
    where f p vcard | p D.@@ "begin" = vcard -- administrative
                    | p D.@@ "end" = vcard   -- junk.
                    | p D.@@ "version" =
                        let D.Text ver = D.prop_value p
                        in vcard { vcard_version = parseVersion ver }
                    | otherwise = vcardMerge p vcard

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show

writeVCard :: VCard -> B.ByteString
writeVCard (VCard ver attrs) =
    B.intercalate "\r\n"
         [ "BEGIN:VCARD"
         , B.intercalate "\r\n" $
           concatMap (map writeAttribute . snd) (version_attr : Map.toList attrs)
         , "END:VCARD" ]
    where version_attr =
              (undefined, [D.Prop { D.prop_type = D.Type Nothing "VERSION"
                                  , D.prop_parameters = []
                                  , D.prop_value = val }])
          val = D.Text $ B.concat [ showBS $ version_major ver, "."
                                  , showBS $ version_minor ver ]


writeAttribute :: Attribute -> B.ByteString
writeAttribute prop =
    if null (D.prop_parameters prop)
    then B.concat [ writeType (D.prop_type prop), ":"
                  , writeValue (D.prop_value prop) ]
    else B.concat [ writeType (D.prop_type prop), ";"
                  , B.concat $ map writeParameter $ D.prop_parameters prop, ":"
                  , writeValue (D.prop_value prop) ]

writeType :: D.Type -> B.ByteString
writeType typ = case D.type_group typ of
                  Just group -> B.concat [group, ".", D.type_name typ]
                  Nothing -> D.type_name typ

writeParameter :: D.Parameter -> B.ByteString
writeParameter param = B.concat [D.param_name param, "=", D.param_value param]

writeValue :: D.Value D.Rfc2425Types -> B.ByteString
writeValue (D.URI v) = showBS v
writeValue (D.Text v) = v
writeValue (D.Date v) = showBS v
writeValue (D.Time v) = showBS v
writeValue (D.DateTime v) = showBS v
writeValue (D.Integer v) = showBS v
writeValue (D.Boolean True) = "TRUE"
writeValue (D.Boolean False) = "FALSE"
writeValue (D.Float v) = showBS v
writeValue _ = error "Don't know how to print this value type."

