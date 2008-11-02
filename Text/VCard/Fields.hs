-- | Field selectors for sequence values, such as in N or ADR attributes.
module Text.VCard.Fields where

import qualified Text.VCard.Types as V
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.ByteString.Lazy.Char8 as B


seqi name prop n | D.IANAValue (V.Sequence seq) <- D.prop_value prop,
                   length seq > n =
                       if D.prop_type prop == D.nakedType name
                       then seq !! n else
                           error $ "Expecting " ++ B.unpack name ++ " property."
                 | otherwise = error "Wrong value."

n_familyName        prop = seqi "n" prop 0
n_givenName         prop = seqi "n" prop 1
n_additionalNames   prop = seqi "n" prop 2
n_honorificPrefixes prop = seqi "n" prop 3
n_honorificSuffixes prop = seqi "n" prop 4

adr_pobox          prop = seqi "adr" prop 0
adr_extendedAdress prop = seqi "adr" prop 1
adr_streetAdress   prop = seqi "adr" prop 2
adr_locality       prop = seqi "adr" prop 3
adr_region         prop = seqi "adr" prop 4
adr_postalCode     prop = seqi "adr" prop 5
adr_country        prop = seqi "adr" prop 6

geo_latitude  prop = seqi "geo" prop 0
geo_longitude prop = seqi "geo" prop 1

org_organizationName  prop = seqi "org" prop 0
org_organizationUnits prop
    | D.IANAValue (V.Sequence (_:seq)) <- D.prop_value prop = seq
    | otherwise = error "Wrong value."
