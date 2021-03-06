{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Pretty where

import Data.ByteString.Lazy (ByteString)
import Text.XML (parseLBS, Document(..))
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.HTML.DOM (parseLT)
import Text.Blaze.Html (toHtml, ToMarkup)
import Data.Default (def)
import Text.XML.Label (labelXMLDocument)
import Data.Text.Lazy.Encoding (decodeUtf8)


prettyHtmlByteString :: ByteString -> String
prettyHtmlByteString = either show (renderHtml . toHtml) . parseLBS def

prettyDocument :: ToMarkup a => a -> String
prettyDocument = renderHtml . toHtml

bytestring2LabeledDocument ::  ByteString -> (Document, Int)
bytestring2LabeledDocument = labelXMLDocument . parseLT . decodeUtf8

bytestring2document ::  ByteString -> Document
bytestring2document = parseLT . decodeUtf8
