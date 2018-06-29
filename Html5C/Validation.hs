{-# LANGUAGE OverloadedStrings #-}

module Html5C.Validation where

-- import Network.HTTP.Types
-- import qualified Data.CaseInsensitive as CI
-- import Network.HTTP.Conduit
-- import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Combinator
import Data.Aeson.Types
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.ByteString.Lazy.Internal
import Data.Monoid

-- import Text.Blaze.Html5 (Html)
-- import Text.Blaze.Internal
-- import Text.Blaze.Html.Renderer.Utf8
-- import qualified Text.Blaze.Html.Renderer.Pretty as PP
-- import qualified Text.Blaze.Html.Renderer.String as PS

-- import Prelude
-- import qualified Prelude as P
-- import Data.List

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Safe (fromJustNote)
import Debug.Trace


checkErrorsInResponse :: ByteString -> Maybe Text
checkErrorsInResponse response = 
    let err_mess = filter isMError $ unVMessage $ fromJustNote "checkErrorsInResponse" 
                   (decode response :: Maybe ValidMessage)
        mkErrMessage = mconcat $ map (("*** Error Message: "<>) . (<>"\n") . unEText) err_mess
    in  if null err_mess
        then Nothing
        else Just mkErrMessage


data MType = MError { unEText :: Text } 
           | MWarning { unWText :: Text } 
           | MInfo { unIText :: Text } 
             deriving  Show
data ValidMessage = VMessage {unVMessage :: [MType]} deriving Show


isMError :: MType -> Bool
isMError (MError _) = True
isMError _          = False


instance FromJSON MType where
    parseJSON (Object v) = do t <- v .: "type"    :: Parser Text
                              m <- v .: "message" :: Parser Text     
                              case t of
                                "error"   -> return $ MError m
                                "warning" -> return $ MWarning m
                                "info"    -> return $ MInfo m
                                mes       -> error  $ "Unknown message: " ++ (show mes)
    parseJSON _          = mzero


instance FromJSON ValidMessage where
    parseJSON (Object v) = v .: "messages" >>= parseJSON >>= (return . VMessage)
    parseJSON _          = mzero                              


test1 = decode "{\"messages\":[{\"type\":\"error\",\"lastLine\":1},{\"type\":\"error\",\"lastLine\":1}]}" :: Maybe ValidMessage
