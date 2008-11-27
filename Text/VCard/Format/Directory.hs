-- |
-- Copyright   : (c) 2008 Mathieu Boespflug
-- License     : LGPL
-- Maintainer  : mboes@tweag.net
-- Stability   : experimental
-- Portability : non-portable

module Text.VCard.Format.Directory
    ( module Text.VCard, readVCards, fromProperties, writeVCard ) where

import Text.VCard
import qualified Text.VCard.Query as Q
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Printf
import Data.List (intercalate)


instance D.PrintValue ExtraValue where
    printValue (Struct xs) =
        D.escape ",;" $ B.intercalate "," $ intercalate [";"] xs
    printValue (Binary blob) = blob
    printValue (PhoneNumber num) = num
    printValue (UTCOffset sign hrs mins) =
        B.pack $ printf "%c%02d:%02d" sign hrs mins
    printValue (SubVCard vc) = D.escape ",;:" $ writeVCard vc

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show

writeVCard :: VCard -> B.ByteString
writeVCard (VCard ver attrs) =
    D.printDirectory' $ [begin, version] ++ concat (Map.elems attrs) ++ [end]
    where attr typ val = D.Prop (D.nakedType typ) [] val
          begin   = D.Prop (D.nakedType "BEGIN") [] (D.Text "VCARD")
          end     = D.Prop (D.nakedType "END") [] (D.Text "VCARD")
          version = D.Prop (D.nakedType "VERSION") [] $
                    D.Text $ B.concat [ showBS $ version_major ver, "."
                                      , showBS $ version_minor ver ]

readVCards :: SourceName -> B.ByteString -> [VCard]
readVCards file =
    map fromProperties . D.groupByBeginEnd . D.parseDirectory' vCardValueParser

parseVersion :: B.ByteString -> Version
parseVersion s = let (majt, mint) = B.break (=='.') s
                     maj = maybe err fst (B.readInt majt)
                     min = maybe err fst (B.readInt (B.drop 1 mint))
                 in Version maj min
    where err = error "Not a valid version number."

fromProperties :: [VProperty] -> VCard
fromProperties =
    foldr f VCard{ vcard_version = undefined, vcard_properties = Map.empty }
    where f p vcard | p D.@@ "begin" = vcard -- administrative
                    | p D.@@ "end" = vcard   -- junk.
                    | p D.@@ "version" =
                        let D.Text ver = D.prop_value p
                        in vcard { vcard_version = parseVersion ver }
                    | otherwise = Q.insert p vcard

fields :: B.ByteString -> [B.ByteString]
fields "" = []
fields s = B.foldr f [B.empty] s
    where f ';'  (xs:xss) = B.empty : xs : xss
          f '\\' ("":xs:xss) = B.cons ';' xs : xss
          f '\\' (xs:xss) | Just ('\\',_) <- B.uncons xs = B.cons '\\' xs : xss
          f x (xs:xss) = B.cons x xs : xss

vCardValueParser :: D.ValueParser ExtraValue
vCardValueParser tps@(_,params)
    | Just [valspec] <- D.lookupParameter "value" params = parserFromSpec valspec tps
    | otherwise = defaultValueParser tps

type ValueSpec = B.ByteString

parserFromSpec :: ValueSpec -> D.ValueParser ExtraValue
parserFromSpec "uri" = D.pa_URI
parserFromSpec "text" = D.pa_text
parserFromSpec "date" = D.pa_date
parserFromSpec "date-time" = D.pa_dateTime
parserFromSpec "integer" = D.pa_integer
parserFromSpec "boolean" = D.pa_boolean
parserFromSpec "float" = D.pa_float
parserFromSpec "binary" = pa_binary
parserFromSpec "phone-number" = pa_phoneNumber
parserFromSpec "utc-offset" = pa_utcOffset
parserFromSpec "vcard" = pa_subVCard

-- | Maps property types to the corresponding default value parser, in the
-- absence of any VALUE parameter.
defaultValueParser :: D.ValueParser ExtraValue
defaultValueParser tps@(typ,_)
    | typ == D.nakedType "photo" = pa_binary tps
    | typ == D.nakedType "bday" = D.pa_date tps
    | typ == D.nakedType "adr" = pa_struct tps
    | typ == D.nakedType "tel" = pa_phoneNumber tps
    | typ == D.nakedType "tz" = pa_utcOffset tps
    | typ == D.nakedType "geo" = pa_struct tps
    | typ == D.nakedType "logo" = pa_binary tps
    | typ == D.nakedType "agent" = pa_subVCard tps
    | typ == D.nakedType "org" = pa_struct tps
    | typ == D.nakedType "rev" = D.pa_dateTime tps
    | typ == D.nakedType "sound" = pa_binary tps
    | typ == D.nakedType "url" = D.pa_URI tps
    | typ == D.nakedType "key" = pa_binary tps
    | otherwise = D.pa_text tps

-- Parsers for vCard specific value specifications.

-- | A variant of RFC 2425 text type where all ';' characters are escaped
-- except those that serve as field delimiters.
pa_struct :: D.ValueParser ExtraValue
pa_struct tps =
    return . D.IANAValue . Struct . map (map untxt . D.pa_text tps) . fields
    where untxt (D.Text s) = s

pa_binary :: D.ValueParser ExtraValue
pa_binary _ = return . D.IANAValue . Binary

pa_phoneNumber :: D.ValueParser ExtraValue
pa_phoneNumber tps s =
    case D.pa_text tps s of [D.Text txt] -> return $ D.IANAValue $ PhoneNumber txt
                            _ -> error "Expecting single value."

pa_utcOffset :: D.ValueParser ExtraValue
pa_utcOffset _ s | [sign,h1,h2,':',m1,m2] <- B.unpack s =
   let hrs = read (h1:h2:[])
       mins = read (m1:m2:[])
   in return $ D.IANAValue $ UTCOffset sign hrs mins

pa_subVCard :: D.ValueParser ExtraValue
pa_subVCard tps =
    return . D.IANAValue . SubVCard . head . readVCards "<>" . text
    where text s = case D.pa_text tps s of [D.Text txt] -> txt
                                           _ -> error "Expecting single value."
