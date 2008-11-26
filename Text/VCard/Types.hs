-- Copyright (C) 2008 Mathieu Boespflug <mboes@tweag.net>
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301, USA.

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

type VCardValue = D.Value ExtraValue
type VProperty = D.Property ExtraValue
type SourceName = String

data VCard = VCard
    { vcard_version :: Version
    , vcard_properties :: Map.Map I.ByteString [VProperty] }
             deriving Show
