-- |
-- Copyright   : (c) 2008 Mathieu Boespflug
-- License     : LGPL
-- Maintainer  : mboes@tweag.net
-- Stability   : experimental
-- Portability : non-portable
--
-- This module reexports most of the modules in the vCard library in
-- additionto the mime-directory library, with the exception of
-- |Text.VCard.Query|, since that library should be imported qualified, as
-- many of the names clash with standard |Prelude| names.
--
-- To use this library, you probably want to import one of the modules in
-- |Text.VCard.Format| only, to parse and serialize vcards in specific formats.
module Text.VCard ( module D , module Text.VCard.Types , module
Text.VCard.Selectors ) where

import Codec.MIME.ContentType.Text.Directory as D
import Text.VCard.Types
import Text.VCard.Selectors
