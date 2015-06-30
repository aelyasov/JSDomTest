module Config.Data where 

import Html5C.Tags

data HtmlGenMode = HtmlSimple | HtmlComplex
                   deriving Show

data HtmlRandomGenConfig = 
    HtmlRandConfig { maxDepth      :: Int
                   , tagsFrequency :: [(Int, HTML_TAG)]
                   , tagsKnown     :: [HTML_TAG] -- TODO: we can include tag's frequency
                   , namesKnown    :: [String]
                   , idsKnown      :: [String]
                   , classesKnown  :: [String]
                   , genMode       :: HtmlGenMode
                   }
    deriving Show
                     
