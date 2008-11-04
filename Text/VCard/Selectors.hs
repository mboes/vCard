-- | Field selectors for struct values, such as in N or ADR attributes.
module Text.VCard.Selectors where

import qualified Text.VCard.Types as V
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.ByteString.Lazy.Char8.Folded as I


seqi name prop n | D.IANAValue (V.Struct seq) <- D.prop_value prop,
                   length seq > n =
                       if D.prop_type prop == D.nakedType name
                       then seq !! n else
                           error $ "Expecting " ++ I.unpack name ++ " property."
                 | otherwise = error "Wrong value."

n_familyName        prop = seqi "N" prop 0
n_givenName         prop = seqi "N" prop 1
n_additionalNames   prop = seqi "N" prop 2
n_honorificPrefixes prop = seqi "N" prop 3
n_honorificSuffixes prop = seqi "N" prop 4

adr_pobox          prop = seqi "ADR" prop 0
adr_extendedAdress prop = seqi "ADR" prop 1
adr_streetAdress   prop = seqi "ADR" prop 2
adr_locality       prop = seqi "ADR" prop 3
adr_region         prop = seqi "ADR" prop 4
adr_postalCode     prop = seqi "ADR" prop 5
adr_country        prop = seqi "ADR" prop 6

geo_latitude  prop = seqi "GEO" prop 0
geo_longitude prop = seqi "GEO" prop 1

org_organizationName  prop = seqi "ORG" prop 0
org_organizationUnits prop
    | D.IANAValue (V.Struct (_:seq)) <- D.prop_value prop = seq
    | otherwise = error "Wrong value."
