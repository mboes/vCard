module Codec.MIME.ContentType.Text.Directory
    ( Directory, Property(..), Type(..), Parameter(..), Value(..)
    , Rfc2425Types
    , ValueParser
    , nakedType, (@@)
    , parseDirectory
    , pa_URI, pa_text, pa_date, pa_dateTime, pa_integer, pa_bool, pa_float, pa_textList
    , many, single) where

import Data.Time
import System.Locale
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Text.Regex.PCRE.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO.Unsafe


type Directory u = [Property u]

data Property u = Prop
    { prop_type :: Type
    , prop_parameters :: [Parameter]
    , prop_value :: Value u }
                  deriving Show

data Type = Type
    { type_group :: Maybe B.ByteString
    , type_name :: B.ByteString }
            deriving Show

instance Eq Type where
    x == y = let f = B.map toLower . type_name
             in f x == f y

-- | Make a property type without any grouping.
nakedType :: B.ByteString -> Type
nakedType name = Type { type_group = Nothing, type_name = name }

(@@) :: Property u -> B.ByteString -> Bool
p @@ name = prop_type p == nakedType name

instance Ord Type where
    compare x y = let f = B.map toLower . type_name
                  in compare (f x) (f y)

data Parameter = Param
    { param_name :: B.ByteString
    , param_value :: B.ByteString }
                 deriving Show

type URI = B.ByteString

data Value u = URI URI
             | Text B.ByteString
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

-- | Instantiate Value with this phantom type to indicate that property types
-- should be none other than those defined in rfc2425.
data Rfc2425Types

-- | The type of parsers for property values, for instance to read an integer
-- property, text property, etc.
type ValueParser u = (Type, [Parameter]) -> B.ByteString -> [Value u]

-- | Break the input into logical lines, unfolding lines that span multiple
-- physical lines.
unfoldLines :: B.ByteString -> [B.ByteString]
unfoldLines "" = []
unfoldLines s =
    let (l, s') = B.break (== '\o15') s
        join = case unfoldLines (B.drop 3 s') of
              [] -> [l]
              cont:rest -> (B.append l cont) : rest
    in case B.unpack s' of
         [] -> [l]
         '\r':'\n':' ':_ -> join
         '\r':'\n':'\t':_ -> join
         '\r':'\n':_ -> l : unfoldLines (B.drop 2 s')
         _ -> error "Malformed input: no LF after a CR."

newtype P a = P { unP :: B.ByteString -> (a, B.ByteString) }

instance Monad P where
    return x = P $ \s -> (x, s)
    m >>= k = P $ \s -> let (a, s') = unP m s in unP (k a) s'

p :: B.ByteString   -- ^ Text of the regular expression.
  -> P B.ByteString -- ^ The matching part of the input.
p pat = P $ \s -> unsafePerformIO $ do
          Right r <- compile compUngreedy execAnchored pat
          Right result <- regexec r s
          return $ case result of
                     Just (_, match, s', _) -> (match, s')
                     Nothing -> error "Parse error."

capture :: B.ByteString     -- ^ Text of the regular expression containing capturing groups.
        -> P [B.ByteString] -- ^ The captured subparts of the input.
capture pat = P $ \s -> unsafePerformIO $ do
                Right r <- compile compUngreedy execAnchored pat
                Right result <- regexec r s
                return $ case result of
                           Just (_, _, s', captures) -> (captures, s')
                           Nothing -> error "Parse error."

parseDirectory :: ValueParser u
               -- ^ Given a Property Type and a list of parameters,
               -- parse a string representation into a Value.
               -> B.ByteString
               -> Directory u
parseDirectory valparse = concatMap (fst . unP (pa_property valparse)) . unfoldLines

-- | Pa_ a string representation into a property. Note that the
-- return type here is actually a list of properties, because we
-- desugar properties whose values are lists into a list of
-- properties, one for each element of the value list.
pa_property :: ValueParser u
              -- ^ Given a Property Type and a list of parameters,
              -- parse a string representation into a (list of) Value.
              -> P [Property u]
pa_property valparse = do
  [groupt, typt, sept] <- capture "(?:((?:[[:alnum:]]|-)+).)?((?:[[:alnum:]]|-)+)(:|;)"
  params <- case B.unpack sept of
              ";" -> pa_parameterList
              ":" -> return []
  rest <- p ".*$"
  let group = if B.null groupt then Nothing else Just groupt
  let typ = Type { type_group = group, type_name = typt }
      prop v = Prop { prop_type = typ
                    , prop_parameters = params
                    , prop_value = v }
  return $ map prop $ valparse (typ, params) rest

pa_parameterList :: P [Parameter]
pa_parameterList = do
  [name, val, qval, sep] <- capture "((?:[[:alnum:]]|-)+)=(?:([^;:,\"]*)|\"([^\"]*)\")(,|:)"
  ps <- case sep of
          "," -> pa_parameterList
          ":" -> return []
  let value = if B.null val then qval else val
  return $ Param { param_name = name, param_value = value } : ps

-- A few canned parsers for value types defined in rfc2425

pa_URI :: B.ByteString -> Value u
pa_URI = Text

-- | Unescape slashes, newlines and commas.
pa_text :: B.ByteString -> Value u
pa_text = Text . head . pa_textList

pa_date :: B.ByteString -> Value u
pa_date =
    Date . readTime defaultTimeLocale (iso8601DateFormat Nothing) . B.unpack

pa_time :: B.ByteString -> Value u
pa_time = Time . utctDayTime . readTime defaultTimeLocale "%T" . B.unpack

pa_dateTime :: B.ByteString -> Value u
pa_dateTime =
    DateTime .
    readTime defaultTimeLocale (iso8601DateFormat (Just "T%T")) .
    B.unpack

pa_integer :: B.ByteString -> Value u
pa_integer = Integer . fst . fromJust . B.readInteger

pa_bool :: B.ByteString -> Value u
pa_bool "TRUE" = Boolean True
pa_bool "FALSE" = Boolean False

pa_float :: B.ByteString -> Value u
pa_float = Float . read . B.unpack

pa_textList :: B.ByteString -> [B.ByteString]
pa_textList "" = []
pa_textList xs = B.foldr f [B.empty] xs
    where f :: Char -> [B.ByteString] -> [B.ByteString]
          f ','  (xs:xss) = B.empty:xs:xss
          f '\\' ("":xs:xss) = (B.cons ',' xs):xss
          f '\\' (xs:xss) | Just ('n',_)  <- B.uncons xs = B.cons '\n' xs : xss
          f '\\' (xs:xss) | Just ('N',_)  <- B.uncons xs = B.cons '\n' xs : xss
          f '\\' (xs:xss) | Just ('\\',_) <- B.uncons xs = B.cons '\\' xs : xss
          f x (xs:xss) = B.cons x xs : xss

-- | Take a parser for single values to a parser for a list of values. This
-- assumes that the separator between values is the "," character.
many :: (B.ByteString -> Value u) -> B.ByteString -> [Value u]
many pa input = map pa $ breakAll input
    where breakAll "" = []
          breakAll xs = ys : breakAll (B.drop 1 zs)
              where (ys, zs) = B.span (/= ',') xs

-- | Convenience function to turn makes a parser for a single value return a
-- singleton list.
single :: (B.ByteString -> Value u) -> B.ByteString -> [Value u]
single pa = (:[]) . pa
