module Codec.MIME.ContentType.Text.Directory
    ( Directory, Property(..), Type(..), Parameter(..), Value(..)
    , Rfc2425Types
    , ValueParser
    , nakedType, (@@)
    , parseDirectory
    , pa_URI, pa_text, pa_date, pa_dateTime, pa_integer, pa_bool, pa_float, pa_textList
    , many) where

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
unfoldLines s = B.foldr f [B.empty] s
    where f '\r' (xs:xss) | Just (h1, xs') <- B.uncons xs,
                            Just (h2, xs'') <- B.uncons xs' =
                            case (h1, h2) of
                                ('\n', ' ') -> xs'':xss
                                ('\n', '\t') -> xs'':xss
                                ('\n', _) -> "":xs':xss
                                _ -> error "Malformed input: no LF after a CR."
                          | otherwise = "":xss
          f x (xs:xss) = B.cons x xs : xss

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

pa_URI :: ValueParser u
pa_URI _ = (:[]) . Text

-- | Unescape slashes, newlines and commas.
pa_text :: ValueParser u
pa_text tps = take 1 . pa_textList tps

pa_date :: ValueParser u
pa_date _ =
    (:[]) . Date . readTime defaultTimeLocale (iso8601DateFormat Nothing) . B.unpack

pa_time :: ValueParser u
pa_time _ =
    (:[]) . Time . utctDayTime . readTime defaultTimeLocale "%T" . B.unpack

pa_dateTime :: ValueParser u
pa_dateTime _ =
    (:[]) . DateTime .
    readTime defaultTimeLocale (iso8601DateFormat (Just "T%T")) .
    B.unpack

pa_integer :: ValueParser u
pa_integer _ = (:[]) . Integer . fst . fromJust . B.readInteger

pa_bool :: ValueParser u
pa_bool _ "TRUE" = [Boolean True]
pa_bool _ "FALSE" = [Boolean False]

pa_float :: ValueParser u
pa_float _ = (:[]) . Float . read . B.unpack

pa_textList :: ValueParser u
pa_textList _ "" = []
pa_textList _ xs = map (Text . B.pack . B.unpack) $ B.foldr f [B.empty] xs
    where f ','  (xs:xss) = B.empty : xs : xss
          f '\\' ("":xs:xss) = B.cons ',' xs : xss
          f '\\' (xs:xss) | Just ('n',_)  <- B.uncons xs = B.cons '\n' xs : xss
          f '\\' (xs:xss) | Just ('N',_)  <- B.uncons xs = B.cons '\n' xs : xss
          f '\\' (xs:xss) | Just ('\\',_) <- B.uncons xs = B.cons '\\' xs : xss
          f x (xs:xss) = B.cons x xs : xss

-- | Take a parser for single values to a parser for a list of values. This
-- assumes that the separator between values is the "," character, and that
-- values do not contain commas themselves.
many :: ValueParser u -> ValueParser u
many pa tps input = map (head . pa tps) $ breakAll input
    where breakAll "" = []
          breakAll xs = ys : breakAll (B.drop 1 zs)
              where (ys, zs) = B.span (/= ',') xs
