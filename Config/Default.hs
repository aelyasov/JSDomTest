module Config.Default where

import Html5C.Tags
import Config.Data
import Config.DefaultTest
import Config.DefaultReal


defHtmlRandGenConf :: [(Int, HTML_TAG)] -> HtmlRandomGenConfig
defHtmlRandGenConf freqTbl = HtmlRandConfig 
                             { maxDepth      = 5
                             , tagsFrequency = freqTbl
                             , tagsKnown     = []
                             , namesKnown    = []
                             , idsKnown      = []
                             , classesKnown  = []
                             , genMode       = HtmlSimple  
                             }


