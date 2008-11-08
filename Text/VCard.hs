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
