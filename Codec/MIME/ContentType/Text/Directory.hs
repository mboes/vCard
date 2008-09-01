module Codec.MIME.ContentType.Text.Directory
    ( Directory, Property(..), Type(..), Parameter(..), Value(..)
    , Rfc2425Types
    , nakedType, (@@), parseDirectory ) where

import Data.Time
import Data.Bits (shiftL)
import Data.Char (toLower, chr, digitToInt, isAscii, isHexDigit)
import Data.List (intersperse)
import System.Locale


type Directory u = [Property u]

data Property u = Prop
    { prop_type :: Type
    , prop_parameters :: [Parameter]
    , prop_value :: Value u }
                  deriving Show

data Type = Type
    { type_group :: Maybe String
    , type_name :: String }
            deriving Show

instance Eq Type where
    x == y = let f = map toLower . type_name
             in f x == f y

-- | Make a property type without any grouping.
nakedType :: String -> Type
nakedType name = Type { type_group = Nothing, type_name = name }

(@@) :: Property u -> String -> Bool
p @@ name = prop_type p == nakedType name

instance Ord Type where
    compare x y = let f = map toLower . type_name
                  in compare (f x) (f y)

data Parameter = Param
    { param_name :: String
    , param_value :: String }
                 deriving Show

type URI = String

data Value u = URI URI
             | Text String
             | Date Day
             | Time DiffTime
             | DateTime UTCTime
             | Integer Integer
             | Boolean Bool
             | Float Float
-- Decode a list of values as a list of properties, since rfc2425
-- considers them to be semantically equivalent.
--           | List (Value u)
             | IANAValue u -- an IANA defined type not part of rfc2425
               deriving (Eq, Show)

-- | Instantiate Value with this phantom type to indicate that
-- property types should be none other than those defined in rfc2425.
data Rfc2425Types

-- | Break the input into logical lines, unfolding lines that span
-- multiple physical lines.
unfoldLines :: String -> [String]
unfoldLines "" = []
unfoldLines s =
    let (l, s') = break (== '\o15') s
    in case s' of
         [] -> [l]
         '\o15':'\o12':' ':s'' -> case unfoldLines s'' of
                                [] -> [l]
                                cont:rest -> (l ++ cont) : rest
         '\o15':'\o12':s'' -> l : unfoldLines s''
         _ -> error "Malformed input: no LF after a CR."

parseDirectory
    :: ((Type, [Parameter]) -> String -> [Value u])
    -- ^ Given a Property Type and a list of parameters, parse a string
    -- representation into a Value.
    -> String
    -> Directory u
parseDirectory valparse = concatMap (parseProperty valparse) . unfoldLines

-- | Parse a string representation into a property. Note that the
-- return type her is actually a list of properties, because we
-- desugar properties whose values are lists into a list of
-- properties, one for each element of the value list.
parseProperty
    :: ((Type, [Parameter]) -> String -> [Value u])
    -- ^ Given a Property Type and a list of parameters, parse a string
    -- representation into a (list of) Value.
    -> String
    -> [Property u]
parseProperty valparse line =
    let (beg, rest) = break (\x -> x == ';' || x == ':') line
        name = parseType beg
        (params, ':':value) = parseParameterListS rest
    in map (\val -> Prop { prop_type = name
                         , prop_parameters = params
                         , prop_value = val }) (valparse (name, params) value)

parseParameterListS :: String -> ([Parameter], String)
parseParameterListS (':':xs) = ([], ':':xs)
parseParameterListS (';':':':xs) = ([], ':':xs)
parseParameterListS (';':xs) = parseParameterListS xs
parseParameterListS xs =
    let (param, rest) = parseParameterS xs
        (params, rest') = parseParameterListS rest
    in (param:params, rest')

parseParameterS :: String -> (Parameter, String)
parseParameterS xs =
    case break (== '=') xs of
      (name, _:'"':rest) | not (null name) ->
          let (qp, _:rest') = break (== '"') rest
              val = decode [qp]
              param = Param { param_name = name, param_value = val }
          in (param, rest')
      (name,_:rest) | not (null name) ->
          let (val, rest') = break (\x -> x == ',' || x == ':') rest
              param = Param { param_name = name, param_value = val }
          in case rest' of
               ',':rest' -> (param, rest')
               _ -> (param, rest')
      _ -> error "Illegal parameter definition."


parseType :: String -> Type
parseType xs = case break (== '.') xs of
                 (a, []) -> Type { type_group = Nothing, type_name = a }
                 (a, _:b)  -> Type { type_group = Just a, type_name = b }

-- The 'decode' and 'dec' functions are adapted from Ian Lynagh's mime-string
-- package.

-- decode is very forgiving, and makes some best guesses
decode :: [String] -> String
decode = dec . concat . intersperse "\n"
       . removeSoftLinebreaks
       . map (dropFromEndWhile is_tab_space)
    where is_tab_space ' ' = True
          is_tab_space '\t' = True
          is_tab_space _ = False
          breakLast "" = ("", "")
          breakLast [x] = ("", [x])
          breakLast (x:xs) = case breakLast xs of
                                 (ys, zs) -> (x:ys, zs)
          removeSoftLinebreaks [] = []
          removeSoftLinebreaks (x:xs)
              = case breakLast x of
                    (x', "=") ->
                        case removeSoftLinebreaks xs of
                            [] -> [x']
                            (y:ys) -> (x' ++ y):ys
                    _ -> x:removeSoftLinebreaks xs
          dropFromEndWhile :: (a -> Bool) -> [a] -> [a]
          dropFromEndWhile p =
              foldr (\x xs -> if null xs && p x then [] else x:xs) []

dec :: String -> String
dec ('=':c1:c2:cs)
    | isAsciiHexDigit c1 && isAsciiHexDigit c2 =
        chr ((digitToInt c1 `shiftL` 4)  + digitToInt c2):dec cs
    where isAsciiHexDigit :: Char -> Bool
          isAsciiHexDigit c = isAscii c && isHexDigit c
dec (c:cs) = c:dec cs
dec "" = ""

-- A few canned parsers for value types defined in rfc2425

parseURI :: String -> URI
parseURI = id

-- | Unescape slashes, newlines and commas.
parseText :: String -> String
parseText = head . parseTextList

parseDate :: String -> UTCTime
parseDate = readTime defaultTimeLocale (iso8601DateFormat Nothing)

parseTime :: String -> DiffTime
parseTime = utctDayTime . readTime defaultTimeLocale "%T"

parseDateTime :: String -> Day
parseDateTime = readTime defaultTimeLocale (iso8601DateFormat (Just "T%T"))

parseInteger :: String -> Integer
parseInteger = read

parseBool :: String -> Bool
parseBool = read

parseFloat :: String -> Float
parseFloat = read

parseTextList :: String -> [String]
parseTextList "" = []
parseTextList xs = foldr f [[]] xs
    where f ','  (xs:xss) = []:xs:xss
          f '\\' ([]:xs:xss) = (',':xs):xss
          f '\\' (('n':xs):xss) = ('\n':xs):xss
          f '\\' (('N':xs):xss) = ('\n':xs):xss
          f '\\' (('\\':xs):xss) = ('\\':xs):xss
          f x (xs:xss) = (x:xs):xss

parseList :: (String -> a) -> [a]
parseList = undefined
