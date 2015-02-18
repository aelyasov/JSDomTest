import Text.XmlHtml
import Data.ByteString.Char8
import Blaze.ByteString.Builder
import Data.Either

-- import qualified Data.ByteString as B
-- import Data.ByteString (ByteString)


test_html = "<!DOCTYPE HTML>\n<html><head><title>Title</title><base href=\".\" target=\"_blank\"></head><body itemscope=\"\" itemtype=\"http://schema.org/WebPage\"><h1><a></a>A2A2</h1><h1></h1></body></html>"

test = case parseHTML "" (pack test_html) of
         Right doc -> toByteString $ render doc
         Left _    -> error "error" 
