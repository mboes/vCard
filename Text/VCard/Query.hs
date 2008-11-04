module Text.VCard.Query where

import Text.VCard.Types
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8.Caseless as I


lookup :: I.ByteString -> VCard -> Maybe [VCardValue]
lookup typ vcard =
    fmap (Prelude.map D.prop_value) $ Map.lookup typ (vcard_attributes vcard)

-- | Unsafe variant that assumes given type name maps to exactly one value in the vCard.
lookup' :: I.ByteString -> VCard -> VCardValue
lookup' typ vcard =
     D.prop_value $ head $ fromJust $ Map.lookup typ (vcard_attributes vcard)

insert :: Attribute -> VCard -> VCard
insert p vcard@(VCard {vcard_attributes = attrs}) =
    vcard{ vcard_attributes = Map.insertWith merge (D.type_name (D.prop_type p)) [p] attrs }
    where merge [p] ps = p:ps

filterWithAttribute :: (Attribute -> Bool) -> [VCard] -> [VCard]
filterWithAttribute f =
    Prelude.filter (not . Map.null . (Map.filter (not . null . Prelude.filter f) . vcard_attributes))

filterWithType :: (D.Type -> VCardValue -> Bool) -> [VCard] -> [VCard]
filterWithType f = filterWithAttribute (\prop -> f (D.prop_type prop) (D.prop_value prop))

filter :: (VCardValue -> Bool) -> [VCard] -> [VCard]
filter f = filterWithAttribute (\prop -> f (D.prop_value prop))

mapWithAttribute :: (Attribute -> VCardValue) -> [VCard] -> [VCard]
mapWithAttribute f = Prelude.map updateVCard
    where updateProp prop = prop { D.prop_value = (f prop) }
          updateVCard vcard =
              vcard { vcard_attributes = Map.map (Prelude.map updateProp) (vcard_attributes vcard) }

mapWithType :: (D.Type -> VCardValue -> VCardValue) -> [VCard] -> [VCard]
mapWithType f = mapWithAttribute (\prop -> f (D.prop_type prop) (D.prop_value prop))

map :: (VCardValue -> VCardValue) -> [VCard] -> [VCard]
map f = mapWithAttribute (\prop -> f (D.prop_value prop))
