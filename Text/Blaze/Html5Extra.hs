{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.Html5Extra 
    ( -- | Tags 
      mainT
    , math
    , bdi
    , dataT
    , s
    , template
    , svg
    , u 
    , dialog
    , menuitemT
    -- | Attributes
    , itemid
    , itemtype  
    , itemscope
    ) where

import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html


mainT :: Html -> Html  
mainT = Parent "main" "<main" "</main>"

math :: Html -> Html  
math = Parent "math" "<math" "</math>"

bdi :: Html -> Html 
bdi = Parent "bdi" "<bdi" "</bdi>"

dataT :: Html -> Html 
dataT = Parent "data" "<data" "</data>"

dialog :: Html -> Html 
dialog = Parent "dialog" "<dialog" "</dialog>"

menuitemT :: Html -> Html 
menuitemT = Parent "menuitem" "<menuitem" "</menuitem>"

s :: Html -> Html 
s = Parent "s" "<s" "</s>"

template :: Html -> Html 
template = Parent "template" "<template" "</template>"

svg :: Html -> Html 
svg = Parent "svg" "<svg" "</svg>"

u :: Html -> Html 
u = Parent "u" "<u" "</u>"

itemid :: AttributeValue -> Attribute
itemid = attribute "itemid" " itempid=\""

itemscope :: AttributeValue -> Attribute
itemscope = attribute "itemscope" " itemscope=\""

itemtype :: AttributeValue -> Attribute
itemtype = attribute "itemtype" " itemtype=\""
