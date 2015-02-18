module Main where

import Control.Monad    (msum)
import Control.Monad.Trans.Class
import Happstack.Server ( Method(GET, POST), dir, method, toResponse
                        , Browsing(EnableBrowsing), nullConf, ok, look
                        , serveDirectory, simpleHTTP, seeOther
                        )
import Html5.Arbitrary
import Language.JavaScript.Parser

main :: IO ()
-- main = simpleHTTP nullConf $ ok "Hello, World!"
-- main = simpleHTTP nullConf $ msum
--        [ do method GET
--             serveDirectory EnableBrowsing ["happstack_test.html"] "."
--        , do method POST
--             flag <- look "getNewDoc"
--             if flag == "yes"
--             then lift getNewHtml >>= ok . toResponse 
--             else ok $ toResponse "false"
--        ]

main = simpleHTTP nullConf $ msum
       [ do method POST
            lift getNewHtml >>= ok . toResponse
       ]

