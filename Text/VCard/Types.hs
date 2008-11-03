module Text.VCard.Types where

import qualified Data.Map as Map
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.ByteString.Lazy.Char8 as B


data Version = Version
    { version_major :: Int
    , version_minor :: Int }
               deriving Show

data ExtraValue = Struct [[B.ByteString]]
                | Binary B.ByteString
                | PhoneNumber B.ByteString
                | UTCOffset { utcOffset_sign :: Char
                            , utcOffset_hours :: Int
                            -- ^ Valid range: 0-23
                            , utcOffset_minutes :: Int
                            -- ^ Valid range: 0-25
                            }
                | SubVCard VCard
                  deriving Show

type VCardValue = D.Value ExtraValue
type Attribute = D.Property ExtraValue
type SourceName = String

data VCard = VCard
    { vcard_version :: Version
    , vcard_attributes :: Map.Map B.ByteString [Attribute] }
             deriving Show

lookup :: B.ByteString -> VCard -> Maybe [VCardValue]
lookup typ vcard =
    fmap (map D.prop_value) $ Map.lookup typ (vcard_attributes vcard)

insert :: Attribute -> VCard -> VCard
insert p vcard@(VCard {vcard_attributes = attrs}) =
    vcard{ vcard_attributes = Map.insertWith merge (D.type_name (D.prop_type p)) [p] attrs }
    where merge [p] ps = p:ps
