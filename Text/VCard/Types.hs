-- |
-- Copyright   : (c) 2008 Mathieu Boespflug
-- License     : LGPL
-- Maintainer  : mboes@tweag.net
-- Stability   : experimental
-- Portability : non-portable

module Text.VCard.Types
    ( Version(..), ExtraValue(..), VCard(..)
    , VCardValue, VProperty, SourceName ) where

import qualified Data.Map as Map
import qualified Codec.MIME.ContentType.Text.Directory as D
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Char8.Caseless as I


data Version = Version
    { version_major :: Int
    , version_minor :: Int }
               deriving Show

-- | Additional value specifications in RFC 2426.
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

-- | A |VCardValue| obeys the union of the value specifications in RFC 2425
-- and RFC 2426.
type VCardValue = D.Value ExtraValue
type VProperty = D.Property ExtraValue
type SourceName = String

data VCard = VCard
    { vcard_version :: Version
    , vcard_properties :: Map.Map I.ByteString [VProperty] }
             deriving Show
