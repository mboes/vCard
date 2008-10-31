module Text.VCard where

import qualified Data.Map as Map
import qualified Codec.MIME.ContentType.Text.Directory as D

data Version = Version
    { version_major :: Int
    , version_minor :: Int }
               deriving Show

type VCardValue = D.Rfc2425Value
type Attribute = D.Property VCardValue
type SourceName = String

data VCard = VCard
    { vcard_version :: Version
    , vcard_attributes :: Map.Map D.Type [Attribute] }
             deriving Show

vcardMerge :: Attribute -> VCard -> VCard
vcardMerge p vcard@(VCard {vcard_attributes = attrs}) =
    vcard{ vcard_attributes = Map.insertWith merge (D.prop_type p) [p] attrs }
    where merge [p] ps = p:ps
