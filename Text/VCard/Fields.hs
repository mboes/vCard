-- | Field selectors for sequence values, such as in N or ADR attributes.
module Text.VCard.Fields where

import qualified Text.VCard.Types as V
import qualified Codec.MIME.ContentType.Text.Directory as D


seqi prop n | D.IANAValue (V.Sequence seq) <- D.prop_value prop,
              length seq > n = seq !! n
            | otherwise = error "Wrong value."

n_familyName        prop = seqi prop 0
n_givenName         prop = seqi prop 1
n_additionalNames   prop = seqi prop 2
n_honorificPrefixes prop = seqi prop 3
n_honorificSuffixes prop = seqi prop 4

adr_pobox          prop = seqi prop 0
adr_extendedAdress prop = seqi prop 1
adr_streetAdress   prop = seqi prop 2
adr_locality       prop = seqi prop 3
adr_region         prop = seqi prop 4
adr_postalCode     prop = seqi prop 5
adr_country        prop = seqi prop 6

geo_latitude  prop = seqi prop 0
geo_longitude prop = seqi prop 1

org_organizationName  prop = seqi prop 0
org_organizationUnits prop
    | D.IANAValue (V.Sequence (_:seq)) <- D.prop_value prop = seq
    | otherwise = error "Wrong value."
