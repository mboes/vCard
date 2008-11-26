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

-- | This module reexports most of the modules in the vCard library in
-- additionto the mime-directory library, with the exception of
-- |Text.VCard.Query|, since that library should be imported qualified, as
-- many of the names clash with standard |Prelude| names.
module Text.VCard
    ( module D
    , module Text.VCard.Types
    , module Text.VCard.Selectors ) where

import Codec.MIME.ContentType.Text.Directory as D
import Text.VCard.Types
import Text.VCard.Selectors
