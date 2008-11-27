-- |
-- Copyright   : (c) 2008 Mathieu Boespflug
-- License     : LGPL
-- Maintainer  : mboes@tweag.net
-- Stability   : experimental
-- Portability : portable

module Text.VCard.Query where

import Text.VCard.Types
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8.Caseless as I


lookup :: I.ByteString -> VCard -> Maybe [VCardValue]
lookup typ vcard =
    fmap (Prelude.map D.prop_value) $ Map.lookup typ (vcard_properties vcard)

-- | Unsafe variant that assumes given type name maps to exactly one value in the vCard.
lookup' :: I.ByteString -> VCard -> VCardValue
lookup' typ vcard =
     D.prop_value $ head $ fromJust $ Map.lookup typ (vcard_properties vcard)

insert :: VProperty -> VCard -> VCard
insert p vcard@(VCard {vcard_properties = attrs}) =
    vcard{ vcard_properties = Map.insertWith merge (D.type_name (D.prop_type p)) [p] attrs }
    where merge [p] ps = p:ps

filterWithProperty :: (VProperty -> Bool) -> [VCard] -> [VCard]
filterWithProperty f =
    Prelude.filter (not . Map.null . (Map.filter (not . null . Prelude.filter f) . vcard_properties))

filterWithType :: (D.Type -> VCardValue -> Bool) -> [VCard] -> [VCard]
filterWithType f = filterWithProperty (\prop -> f (D.prop_type prop) (D.prop_value prop))

filter :: (VCardValue -> Bool) -> [VCard] -> [VCard]
filter f = filterWithProperty (\prop -> f (D.prop_value prop))

mapWithProperty :: (VProperty -> VCardValue) -> [VCard] -> [VCard]
mapWithProperty f = Prelude.map updateVCard
    where updateProp prop = prop { D.prop_value = (f prop) }
          updateVCard vcard =
              vcard { vcard_properties = Map.map (Prelude.map updateProp) (vcard_properties vcard) }

mapWithType :: (D.Type -> VCardValue -> VCardValue) -> [VCard] -> [VCard]
mapWithType f = mapWithProperty (\prop -> f (D.prop_type prop) (D.prop_value prop))

map :: (VCardValue -> VCardValue) -> [VCard] -> [VCard]
map f = mapWithProperty (\prop -> f (D.prop_value prop))
