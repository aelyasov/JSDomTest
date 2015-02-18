module Html5.Context where

import Html5.Tags
import Util.Stack
import Data.List
-- import Html5.Generators

import Text.Blaze.Html5 hiding (map)
import Html5.QuickCheck.Gen

metadataContent :: [HTML_TAG]
metadataContent = [TAG_BASE, TAG_LINK, TAG_META, TAG_NOSCRIPT, TAG_SCRIPT, TAG_STYLE, TAG_TEMPLATE, TAG_TITLE]

flowContent :: [HTML_TAG]
-- flowContent = [TAG_MAIN, TAG_ABBR]
flowContent = [TAG_A, TAG_ABBR, TAG_ADDRESS, TAG_AREA, TAG_ASIDE, TAG_ARTICLE, TAG_SECTION, TAG_NAV, TAG_H1, TAG_H2, TAG_H3, TAG_H4, TAG_H5, TAG_H6, TAG_HEADER, TAG_HGROUP, TAG_HR, TAG_AUDIO, TAG_B, TAG_BDI, TAG_BDO, TAG_BLOCKQUOTE, TAG_BR, TAG_BUTTON, TAG_CANVAS, TAG_CITE, TAG_CODE, TAG_DATA, TAG_DATALIST, TAG_DEL, TAG_DETAILS, TAG_DFN, TAG_DIALOG, TAG_DIV, TAG_DL, TAG_EM, TAG_EMBED, TAG_FIELDSET, TAG_FIGURE, TAG_FOOTER, TAG_FORM, TAG_I, TAG_IFRAME, TAG_IMG, TAG_INPUT, TAG_INS, TAG_KBD, TAG_KEYGEN, TAG_LABEL, TAG_LINK,TAG_MAIN, TAG_MAP, TAG_MARK, TAG_MATH, TAG_META, TAG_METER, TAG_NOSCRIPT, TAG_OBJECT, TAG_OL, TAG_OUTPUT, TAG_P, TAG_PRE, TAG_PROGRESS, TAG_Q, TAG_RUBY, TAG_S, TAG_SAMP, TAG_SCRIPT, TAG_SELECT, TAG_SMALL, TAG_TEMPLATE, TAG_SPAN,  TAG_STRONG,  TAG_SUB, TAG_SUP, TAG_SVG, TAG_TABLE, TAG_TEMPLATE, TAG_TEXTAREA, TAG_TIME, TAG_U, TAG_UL, TAG_VAR, TAG_VIDEO, TAG_WBR]
flowContent1 = [TAG_A, TAG_ABBR, TAG_ADDRESS, TAG_AREA, TAG_ARTICLE, TAG_ASIDE, TAG_AUDIO, TAG_B, TAG_BDI, TAG_BDO, TAG_BLOCKQUOTE, TAG_BR, TAG_BUTTON, TAG_CANVAS, TAG_CITE, TAG_CODE, TAG_DATA, TAG_DATALIST, TAG_DEL, TAG_DETAILS, TAG_DFN, TAG_DIALOG, TAG_DIV, TAG_DL, TAG_EM, TAG_EMBED, TAG_FIELDSET, TAG_FIGURE, TAG_FOOTER, TAG_FORM, TAG_H1, TAG_H2, TAG_H3, TAG_H4, TAG_H5, TAG_H6, TAG_HEADER, TAG_HGROUP, TAG_HR, TAG_I, TAG_IFRAME, TAG_IMG, TAG_INPUT, TAG_INS, TAG_KBD, TAG_KEYGEN, TAG_LABEL, TAG_LINK, TAG_MAIN, TAG_MAP, TAG_MARK, TAG_MATH, TAG_MENU, TAG_META, TAG_METER, TAG_NAV, TAG_NOSCRIPT, TAG_OBJECT, TAG_OL, TAG_OUTPUT, TAG_P, TAG_PRE, TAG_PROGRESS, TAG_Q, TAG_RUBY, TAG_S, TAG_SAMP, TAG_SCRIPT, TAG_SECTION, TAG_SELECT, TAG_SMALL, TAG_SPAN, TAG_STRONG, TAG_STYLE, TAG_SUB, TAG_SUP, TAG_SVG, TAG_TABLE, TAG_TEMPLATE, TAG_TEXTAREA, TAG_TIME, TAG_U, TAG_UL, TAG_VAR, TAG_VIDEO, TAG_WBR]

sectioningContent :: [HTML_TAG]
sectioningContent = [TAG_ARTICLE, TAG_ASIDE, TAG_NAV, TAG_SECTION]

headingContent :: [HTML_TAG] 
headingContent = [TAG_H1, TAG_H2, TAG_H3, TAG_H4, TAG_H5, TAG_H6, TAG_HGROUP]

-- | Excluded tags: TAG_CANVAS
phrasingContent :: [HTML_TAG]
-- phrasingContent = [TAG_ABBR]
phrasingContent = [TAG_A, TAG_ABBR, TAG_AREA, TAG_AUDIO, TAG_B, TAG_BDI, TAG_BDO, TAG_BR, TAG_BUTTON, TAG_CANVAS, TAG_CITE, TAG_CODE, TAG_DATA, TAG_DATALIST, TAG_DEL, TAG_DFN, TAG_EM, TAG_EMBED, TAG_I, TAG_IFRAME, TAG_IMG, TAG_INPUT, TAG_INS, TAG_KBD, TAG_KEYGEN, TAG_LABEL, TAG_LINK, TAG_MAP, TAG_MARK, TAG_MATH, TAG_META, TAG_METER, TAG_NOSCRIPT, TAG_OBJECT, TAG_OUTPUT, TAG_PROGRESS, TAG_Q, TAG_RUBY, TAG_S, TAG_SAMP, TAG_SCRIPT, TAG_SELECT, TAG_SMALL, TAG_SPAN,  TAG_STRONG, TAG_SUB, TAG_SUP, TAG_SVG, TAG_TEMPLATE, TAG_TEXTAREA, TAG_TIME, TAG_U, TAG_VAR, TAG_VIDEO, TAG_WBR]
phrasingContent1 = [TAG_A, TAG_ABBR, TAG_AREA, TAG_AUDIO, TAG_B, TAG_BDI, TAG_BDO, TAG_BR, TAG_BUTTON, TAG_CANVAS, TAG_CITE, TAG_CODE, TAG_DATA, TAG_DATALIST, TAG_DEL, TAG_DFN, TAG_EM, TAG_EMBED, TAG_I, TAG_IFRAME, TAG_IMG, TAG_INPUT, TAG_INS, TAG_KBD, TAG_KEYGEN, TAG_LABEL, TAG_LINK, TAG_MAP, TAG_MARK, TAG_MATH, TAG_META, TAG_METER, TAG_NOSCRIPT, TAG_OBJECT, TAG_OUTPUT, TAG_PROGRESS, TAG_Q, TAG_RUBY, TAG_S, TAG_SAMP, TAG_SCRIPT, TAG_SELECT, TAG_SMALL, TAG_SPAN, TAG_STRONG, TAG_SUB, TAG_SUP, TAG_SVG, TAG_TEMPLATE, TAG_TEXTAREA, TAG_TIME, TAG_U, TAG_VAR, TAG_VIDEO, TAG_WBR]

embeddedContent :: [HTML_TAG]
embeddedContent = [TAG_AUDIO, TAG_CANVAS, TAG_EMBED, TAG_IFRAME, TAG_IMG, TAG_MATH, TAG_OBJECT, TAG_SVG, TAG_VIDEO]

interactiveContent :: [HTML_TAG]
interactiveContent = [TAG_A, TAG_AUDIO, TAG_BUTTON, TAG_DETAILS, TAG_EMBED, TAG_IFRAME, TAG_IMG, TAG_INPUT, TAG_KEYGEN, TAG_LABEL, TAG_OBJECT, TAG_SELECT, TAG_TEXTAREA, TAG_VIDEO]
-- interactiveContent = [TAG_A, TAG_AUDIO, TAG_BUTTON, TAG_DETAILS, TAG_EMBED, TAG_IFRAME, TAG_IMG, TAG_INPUT, TAG_KEYGEN, TAG_LABEL, TAG_OBJECT, TAG_SELECT, TAG_TEXTAREA, TAG_VIDEO]

palpableContent :: [HTML_TAG]
palpableContent = [TAG_A, TAG_ABBR, TAG_ADDRESS, TAG_ARTICLE, TAG_ASIDE, TAG_AUDIO, TAG_B, TAG_BDI, TAG_BDO, TAG_BLOCKQUOTE, TAG_BUTTON, TAG_CANVAS, TAG_CITE, TAG_CODE, TAG_DATA, TAG_DETAILS, TAG_DFN, TAG_DIV, TAG_DL, TAG_EM, TAG_EMBED, TAG_FIELDSET, TAG_FIGURE, TAG_FOOTER, TAG_FORM, TAG_H1, TAG_H2, TAG_H3, TAG_H4, TAG_H5, TAG_H6, TAG_HEADER, TAG_HGROUP, TAG_I, TAG_IFRAME, TAG_IMG, TAG_INPUT, TAG_INS, TAG_KBD, TAG_KEYGEN, TAG_LABEL, TAG_MAIN, TAG_MAP, TAG_MARK, TAG_MATH, TAG_MENU, TAG_METER, TAG_NAV, TAG_OBJECT, TAG_OL, TAG_OUTPUT, TAG_P, TAG_PRE, TAG_PROGRESS, TAG_Q, TAG_RUBY, TAG_S, TAG_SAMP, TAG_SECTION,  TAG_SELECT, TAG_SMALL, TAG_SPAN, TAG_STRONG, TAG_SUB, TAG_SUP, TAG_SVG, TAG_TABLE, TAG_TEXTAREA, TAG_TIME, TAG_U, TAG_UL, TAG_VAR, TAG_VIDEO]

labelableElements :: [HTML_TAG]
labelableElements = [TAG_BUTTON, TAG_INPUT, TAG_KEYGEN, TAG_METER, TAG_OUTPUT, TAG_PROGRESS, TAG_SELECT, TAG_TEXTAREA] 

scriptSupportingElements :: [HTML_TAG]
scriptSupportingElements = [TAG_SCRIPT, TAG_TEMPLATE]

mediaElements :: [HTML_TAG]
mediaElements = [TAG_VIDEO, TAG_AUDIO]


cmodel2tags :: CModel -> [HTML_TAG]
cmodel2tags CFlow          = flowContent
-- cmodel2tags CFlowNoMain    = flowContent \\ [TAG_MAIN]
cmodel2tags CHeading       = headingContent
cmodel2tags CSectioning    = sectioningContent
cmodel2tags CMetadata      = metadataContent
cmodel2tags CInteractive   = interactiveContent
-- cmodel2tags CNoInteractive = phrasingContent \\ interactiveContent
cmodel2tags CPhrasing      = phrasingContent
cmodel2tags CEmbedded      = embeddedContent
cmodel2tags CPalpable      = palpableContent
cmodel2tags CScript        = error "cmodel2tags: undefined tags"
-- cmodel2tags CTransparent   = error "cmodel2tags: undefined tags"
cmodel2tags CParagraph     = error "cmodel2tags: undefined tags"
cmodel2tags CLabelable     = labelableElements
cmodel2tags CScSupporting  = scriptSupportingElements
cmodel2tags CMedia         = mediaElements
cmodel2tags (CExcl tags)   = tags
cmodel2tags (CIncl tags)   = tags


type Context = Stack CModel 

data CModel = CFlow
            | CHeading 
            | CSectioning
            | CMetadata
            | CInteractive
            | CPhrasing
            | CEmbedded
            | CPalpable
            | CScript
            | CParagraph
            | CLabelable
            | CScSupporting
            | CMedia
            | CExcl [HTML_TAG]
            | CIncl [HTML_TAG]
              deriving (Show, Eq)


isCExclude :: CModel -> Bool
isCExclude (CExcl _) = True
isCExclude _         = False        

isCInclude :: CModel -> Bool
isCInclude (CIncl _) = True
isCInclude _         = False        

isContext  :: CModel -> Bool
isContext (CIncl _) = False
isContext (CExcl _) = False
isContext _         = True

findTagCtx :: HTML_TAG -> Context -> Bool
findTagCtx tag ctx | not $ isEmpty ctx = 
                       let (c, ctx') = pop ctx
                       in  if isCInclude c
                           then case c of
                                  CIncl [tg] | tg == tag -> True
                                             | otherwise -> findTagCtx tag ctx'
                           else findTagCtx tag ctx'
                   | otherwise         = False


getFstCtx :: Context -> Maybe CModel
getFstCtx ctx | not $ isEmpty ctx = let (c, ctx') = pop ctx
                                         in  if isContext c
                                             then Just c
                                             else getFstCtx ctx'  
              | otherwise         = Nothing



mkGens :: Context -> Int -> [Context -> Int -> GenState HasMain Html] -> [GenState HasMain Html]
mkGens cm n gens  = Prelude.map (\g ->  g cm n) gens       

