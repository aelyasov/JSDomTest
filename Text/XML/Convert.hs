module Text.XML.Convert (document2bytestring) where

import Text.Blaze (toMarkup)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.XML (Document)
import Data.ByteString.Lazy (ByteString)

document2bytestring :: Document -> ByteString
document2bytestring = renderHtml . toMarkup

