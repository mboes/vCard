-- |
-- Copyright   : (c) 2008 Mathieu Boespflug
-- License     : LGPL
-- Maintainer  : mboes@tweag.net
-- Stability   : experimental
-- Portability : portable
--
-- Convenience functions to extract values in vCards and mapping over
-- these values.
module Text.VCard.Query where

import Text.VCard.Types
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8.Caseless as I


-- | Analog to |Data.List.lookup|. In general an attribute may map to a number
-- of values. vCards should maintain the invariant that @forall attr, lookup
-- attr vcard /= Just []@.
lookup :: I.ByteString -> VCard -> Maybe [VCardValue]
lookup typ vcard =
    fmap (Prelude.map D.prop_value) $ Map.lookup typ (vcard_properties vcard)

-- | Unsafe variant of |lookup| that assumes given type name maps to exactly
-- one value in the vCard.
lookup' :: I.ByteString -> VCard -> VCardValue
lookup' typ vcard =
     D.prop_value $ head $ fromJust $ Map.lookup typ (vcard_properties vcard)

insert :: VProperty -> VCard -> VCard
insert p vcard@(VCard {vcard_properties = attrs}) =
    vcard{ vcard_properties = Map.insertWith merge (D.type_name (D.prop_type p)) [p] attrs }
    where merge [p] ps = p:ps

-- | Supports the most general filter predicates of the @filter@ family of functions.
filterWithProperty :: (VProperty -> Bool) -> [VCard] -> [VCard]
filterWithProperty f =
    Prelude.filter (not . Map.null . (Map.filter (not . null . Prelude.filter f) . vcard_properties))

-- | A specialization of |filterWithProperty|.
filterWithType :: (D.Type -> VCardValue -> Bool) -> [VCard] -> [VCard]
filterWithType f = filterWithProperty (\prop -> f (D.prop_type prop) (D.prop_value prop))

-- | Filter by property value only.
filter :: (VCardValue -> Bool) -> [VCard] -> [VCard]
filter f = filterWithProperty (\prop -> f (D.prop_value prop))

-- | Supports the most general map predicates of the @map@ family of functions.
mapWithProperty :: (VProperty -> VCardValue) -> [VCard] -> [VCard]
mapWithProperty f = Prelude.map updateVCard
    where updateProp prop = prop { D.prop_value = (f prop) }
          updateVCard vcard =
              vcard { vcard_properties = Map.map (Prelude.map updateProp) (vcard_properties vcard) }

-- | A specialization of |mapWithProperty|.
mapWithType :: (D.Type -> VCardValue -> VCardValue) -> [VCard] -> [VCard]
mapWithType f = mapWithProperty (\prop -> f (D.prop_type prop) (D.prop_value prop))

-- | Map by property value only.
map :: (VCardValue -> VCardValue) -> [VCard] -> [VCard]
map f = mapWithProperty (\prop -> f (D.prop_value prop))
