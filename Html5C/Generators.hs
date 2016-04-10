{-# LANGUAGE DoAndIfThenElse #-}

module Html5C.Generators where

import Prelude
import qualified Prelude as P

import Control.Monad
import Data.Monoid
import Data.List
import Data.Either
import Data.Maybe
import Util.Stack
import Util.Iterate
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Html5C.Context
import Html5C.Tags

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen 

import Text.Blaze.Html5
import Text.Blaze.Html5Extra
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal
import qualified Text.Blaze.Internal as BI
import Text.Blaze.Html.Renderer.Pretty

import Html5C.QuickCheck.Gen

import Util.Debug
import Debug.Trace
import Safe (fromJustNote)


fromTag2Gen :: HTML_TAG -> GenHtmlState
fromTag2Gen TAG_A          = genTAG_a 
fromTag2Gen TAG_ABBR       = genTAG_abbr
fromTag2Gen TAG_ADDRESS    = genTAG_address
fromTag2Gen TAG_AREA       = genTAG_area
fromTag2Gen TAG_ARTICLE    = genTAG_article
fromTag2Gen TAG_ASIDE      = genTAG_aside 
fromTag2Gen TAG_AUDIO      = genTAG_audio
fromTag2Gen TAG_B          = genTAG_b
fromTag2Gen TAG_BASE       = genTAG_base
fromTag2Gen TAG_BDI        = genTAG_bdi
fromTag2Gen TAG_BDO        = genTAG_bdo
fromTag2Gen TAG_BLOCKQUOTE = genTAG_blockquote
fromTag2Gen TAG_BR         = genTAG_br
fromTag2Gen TAG_BUTTON     = genTAG_button
fromTag2Gen TAG_CANVAS     = genTAG_canvas
fromTag2Gen TAG_CAPTION    = genTAG_caption
fromTag2Gen TAG_CITE       = genTAG_cite
fromTag2Gen TAG_CODE       = genTAG_code
-- fromTag2Gen TAG_COL        = genTAG_col 
fromTag2Gen TAG_COLGROUP   = genTAG_colgroup
fromTag2Gen TAG_DATA       = genTAG_data
fromTag2Gen TAG_DATE       = genTAG_date
fromTag2Gen TAG_DATALIST   = genTAG_datalist
fromTag2Gen TAG_DEL        = genTAG_del
fromTag2Gen TAG_DETAILS    = genTAG_details
fromTag2Gen TAG_DFN        = genTAG_dfn
fromTag2Gen TAG_DIALOG     = genTAG_dialog
fromTag2Gen TAG_DIV        = genTAG_div
fromTag2Gen TAG_DL         = genTAG_dl
fromTag2Gen TAG_DD         = genTAG_dd
fromTag2Gen TAG_DT         = genTAG_dt
fromTag2Gen TAG_EM         = genTAG_em
fromTag2Gen TAG_EMBED      = genTAG_embed
fromTag2Gen TAG_FIELDSET   = genTAG_fieldset
fromTag2Gen TAG_FIGURE     = genTAG_figure
fromTag2Gen TAG_FIGCAPTION = genTAG_figcaption
fromTag2Gen TAG_FOOTER     = genTAG_footer
fromTag2Gen TAG_FORM       = genTAG_form
fromTag2Gen TAG_H1         = genTAG_h1
fromTag2Gen TAG_H2         = genTAG_h2
fromTag2Gen TAG_H3         = genTAG_h3
fromTag2Gen TAG_H4         = genTAG_h4
fromTag2Gen TAG_H5         = genTAG_h5
fromTag2Gen TAG_H6         = genTAG_h6
fromTag2Gen TAG_HEADER     = genTAG_header
fromTag2Gen TAG_HGROUP     = genTAG_hgroup
fromTag2Gen TAG_HR         = genTAG_hr
fromTag2Gen TAG_I          = genTAG_i
fromTag2Gen TAG_IFRAME     = genTAG_iframe
fromTag2Gen TAG_IMG        = genTAG_img
fromTag2Gen TAG_INPUT      = genTAG_input
fromTag2Gen TAG_INS        = genTAG_ins
fromTag2Gen TAG_KBD        = genTAG_kbd
fromTag2Gen TAG_KEYGEN     = genTAG_keygen
fromTag2Gen TAG_LABEL      = genTAG_label
fromTag2Gen TAG_LEGEND     = genTAG_legend
fromTag2Gen TAG_LINK       = genTAG_link
fromTag2Gen TAG_LI         = genTAG_li
fromTag2Gen TAG_MAIN       = genTAG_main
fromTag2Gen TAG_MAP        = genTAG_map
fromTag2Gen TAG_MARK       = genTAG_mark
fromTag2Gen TAG_MATH       = genTAG_math
fromTag2Gen TAG_MENU       = genTAG_menu
fromTag2Gen TAG_MENUITEM   = genTAG_menuitem
fromTag2Gen TAG_META       = genTAG_meta
fromTag2Gen TAG_METER      = genTAG_meter
fromTag2Gen TAG_NAV        = genTAG_nav
fromTag2Gen TAG_NOSCRIPT   = genTAG_noscript
fromTag2Gen TAG_OBJECT     = genTAG_object
fromTag2Gen TAG_OL         = genTAG_ol
fromTag2Gen TAG_OPTION     = genTAG_option
fromTag2Gen TAG_OPTGROUP   = genTAG_optgroup
fromTag2Gen TAG_OUTPUT     = genTAG_output
fromTag2Gen TAG_P          = genTAG_p
fromTag2Gen TAG_PARAM      = genTAG_param
fromTag2Gen TAG_PRE        = genTAG_pre
fromTag2Gen TAG_PROGRESS   = genTAG_progress
fromTag2Gen TAG_Q          = genTAG_q
fromTag2Gen TAG_RP         = genTAG_rp
fromTag2Gen TAG_RT         = genTAG_rt
fromTag2Gen TAG_RUBY       = genTAG_ruby
fromTag2Gen TAG_S          = genTAG_s
fromTag2Gen TAG_SAMP       = genTAG_samp
fromTag2Gen TAG_SCRIPT     = genTAG_script
fromTag2Gen TAG_SECTION    = genTAG_section
fromTag2Gen TAG_SELECT     = genTAG_select
fromTag2Gen TAG_SMALL      = genTAG_small
fromTag2Gen TAG_SOURCE     = genTAG_source
fromTag2Gen TAG_SPAN       = genTAG_span
fromTag2Gen TAG_STRONG     = genTAG_strong
fromTag2Gen TAG_STYLE      = genTAG_style
fromTag2Gen TAG_SUB        = genTAG_sub
fromTag2Gen TAG_SUP        = genTAG_sup
fromTag2Gen TAG_SUMMARY    = genTAG_summary
fromTag2Gen TAG_SVG        = genTAG_svg
fromTag2Gen TAG_TR         = genTAG_tr
fromTag2Gen TAG_TD         = genTAG_td
fromTag2Gen TAG_TH         = genTAG_th
fromTag2Gen TAG_TABLE      = genTAG_table
fromTag2Gen TAG_TEMPLATE   = genTAG_template
fromTag2Gen TAG_TEXTAREA   = genTAG_textarea
fromTag2Gen TAG_THEAD      = genTAG_thead
fromTag2Gen TAG_TIME       = genTAG_time
fromTag2Gen TAG_TITLE      = genTAG_title
fromTag2Gen TAG_TFOOT      = genTAG_tfoot
fromTag2Gen TAG_TBODY      = genTAG_tbody
fromTag2Gen TAG_TRACK      = genTAG_track 
fromTag2Gen TAG_U          = genTAG_u
fromTag2Gen TAG_UL         = genTAG_ul
fromTag2Gen TAG_VAR        = genTAG_var
fromTag2Gen TAG_VIDEO      = genTAG_video
fromTag2Gen TAG_WBR        = genTAG_wbr
fromTag2Gen t              = error $ "unknown tag: " ++ show t


-- | The function context2gens takes the tag's frequency table, list of expected tags, and context and returns the weighted list of tag generators
context2gens :: [(Int, HTML_TAG)] -> [HTML_TAG] -> Context -> [(Int, GenHtmlState)]
context2gens freqTbl tags ctx = 
    let ctxL    = stack2list ctx
        negCtx  = P.map cmodel2tags $ filter isCExclude ctxL 
        posCtx  = P.map cmodel2tags $ filter isContext  ctxL
        passCtx = P.map cmodel2tags $ filter isCInclude ctxL
        ctxUni  = (foldl (\\) (foldr1 intersect posCtx) negCtx) ++ concat passCtx
        ctxUniLikely = let ctxUniLikely' = ctxUni `intersect` tags
                       in  if P.null ctxUniLikely'
                           then ctxUni
                           -- then ctxUniLikely'     
                           else ctxUniLikely'
        findTag ts x = find (\(i, h) -> x == h) ts
        tagsFreq     = mapMaybe (findTag freqTbl) $ nub ctxUniLikely 
        
    in  if P.null tagsFreq 
        then trace ("*** Warning: the context is empty " ++ show ctx) $ 
             P.map (\(i, t) -> (i, fromTag2Gen t)) tagsFreq
        else P.map (\(i, t) -> (i, fromTag2Gen t)) tagsFreq   

arbHtmlDefaultHead :: GenHtmlState
arbHtmlDefaultHead = return $ H.head $ H.title $ toHtml "Test Title"

arbHtmlHTML :: GenHtmlState
arbHtmlHTML = do
  st <- get
  let n       = getDepth st
      ctx     = getCtx st
      main_   = hasMain st
      hasHead = hasDefHead st    
  head_ <- put st{ getDepth = iter n, getCtx = push CMetadata ctx}
           >>
           if hasHead
           then arbHtmlDefaultHead
           else arbHtmlHEAD
  body_ <- put st{ getDepth = iter n, getCtx = push CFlow ctx} >> arbHtmlBODY
  if n == 0
  then lift $ return $ toHtml "HTML"
  else lift $ return $ docTypeHtml $ head_ >> body_


arbHtmlHEAD :: GenHtmlState
arbHtmlHEAD = do  
  st <- get
  let n   = getDepth st
      ctx = getCtx st
  metadata_ <- listOfState $ put st{ getDepth = iter n} >> arbHtmlMETADATA
  base_     <- optional genTAG_base
  title_    <- genTAG_title
  if n == 0
  then lift $ return $ toHtml "head"
  else lift $ return $ H.head $ title_ >> base_ >> (mconcat metadata_)

genTAG_base :: GenHtmlState
genTAG_base = debug "genTAG_base" $ lift $ return $ base ! href (toValue ".") ! target (toValue "_blank")


arbHtmlMETADATA :: GenHtmlState
arbHtmlMETADATA = debug "arbHtmlMETADATA" $ do
  st <- get
  let n = getDepth st
  meta_ <- sequence [ genTAG_link
                    , genTAG_style
                    , genTAG_meta
                    , genTAG_noscript
                    , genTAG_script
                    ]
  if n == 0
  then lift $ return Empty
  else lift $ elements meta_ 


genTAG_link :: GenHtmlState
genTAG_link = debug "genTAG_link" $ do
  st <- get
  case getFstCtx $ getCtx st of 
    Just CMetadata ->  lift $ return $ link ! rel (toValue "alternate") ! href (toValue ".")
    Just c | c == CPhrasing || c == CFlow ->  lift $ return $ link ! href (toValue "href") !  itemprop (toValue "url") 
           | otherwise -> error $ "*** genTAG_link " ++ show c
    Nothing        -> error $ "genTAG_link: there is no right context"

genTAG_style :: GenHtmlState
genTAG_style = debug "genTAG_style" $ lift $ return $ H.style Empty

genTAG_meta :: GenHtmlState
genTAG_meta = debug "genTAG_meta" $ do
  st <- get
  case getFstCtx $ getCtx st of
    Just CMetadata -> 
        do nm <- lift $ elements [ "keywords"
                                 , "generator"
                                 , "description"
                                 , "author"
                                 , "application-name"]
           httpEq <- lift $ elements ["refresh"]
           lift $ frequency [ (1, return $ meta 
                               ! name (toValue nm) 
                               ! content (toValue "c"))
                            , (1, return $ meta 
                               ! httpEquiv (toValue httpEq) 
                               ! content (toValue "10"))]
    Just c | c == CPhrasing || c == CFlow -> 
               lift $ return $ meta 
                        !  content (toValue "c") 
                        !  itemprop (toValue "url")
           | otherwise  -> error $ "*** genTAG_meta " ++ show c
    Nothing        -> error $ "genTAG_link: there is no right context"


genTAG_noscript :: GenHtmlState
genTAG_noscript = debug "genTAG_noscript" $ do
  st <- get
  let ctx   = getCtx st 
      depth = getDepth st
  if depth == 0
  then lift $ return $ toHtml "NOSCRIPT1"
  else case getFstCtx ctx of
    Just CMetadata -> lift $ return $ noscript Empty
    Just c | c == CPhrasing || c == CFlow -> 
               do html_ <- put st{getDepth = iter depth, getCtx = push (CExcl [TAG_NOSCRIPT]) ctx} >> arbPhrasingContent
                  lift $ frequency [ (1, return $ toHtml "NOSCRIPT2")
                                   , (4, return $ noscript html_)]
           | otherwise -> error $ "*** genTAG_noscript " ++ show c
    Nothing        -> error $ "genTAG_noscript: there is no right context"


genTAG_script :: GenHtmlState
genTAG_script = debug "genTAG_script" $ lift $ return $ script $ toHtml "SCRIPT"

genTAG_title :: GenHtmlState
genTAG_title = debug "genTAG_title" $ lift $ return $ H.title $ toHtml "Title"


genTAG_template :: GenHtmlState
genTAG_template = debug "genTAG_template" $ lift $ return $ template $ toHtml "TEMPLATE"


arbHtmlBODY :: GenHtmlState
arbHtmlBODY = debug "arbHtmlBODY" $ do
  st <- get
  let ctx   = getCtx st 
      depth = getDepth st
  if depth == 0
  then lift $ return $ toHtml "BODY"
  else do html_ <- put st{getDepth = iter depth, getCtx = push CFlow ctx} >> arbFlowContent
          lift $ return $ (body html_) ! itemscope (toValue "") ! itemtype (toValue "http://schema.org/WebPage")


arbFlowContent :: GenHtmlState
-- arbFlowContent _  0  = lift $ return $ toHtml $ "FLOW CONTENT"
arbFlowContent = debug "arbFlowContent" $ do 
  st <- get
  let -- main_   = hasMain    st 
      ctx     = getCtx     st
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml $ "FLOW CONTENT"
  else do flow_ <- resizeState ndegree $ 
                   listOfState $ 
                   do st' <- get
                      let main_ = hasMain st'                         
                      if not main_ 
                      then do put st'{getCtx = ctx} 
                              frequencyState $ context2gens freqTbl tags ctx
                      else 
                           do put st'{getCtx = push (CExcl [TAG_MAIN]) ctx} 
                              frequencyState $ context2gens freqTbl tags $ push (CExcl [TAG_MAIN]) ctx
          lift $ return $ mconcat flow_


arbPhrasingContent :: GenHtmlState
arbPhrasingContent = debug "arbPhrasingContent" $ do
  st <- get
  let -- main_   = hasMain    st 
      ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml $ "PHRASING CONTENT"
  else do flow_ <- resizeState ndegree $ 
                   listOfState $ 
                   do st' <- get
                      let main_ = hasMain st' 
                      if not main_
                   -- if main_          
                      then do  put st'{getCtx = ctx} 
                               frequencyState $ context2gens freqTbl tags ctx
                      else 
                           do put st'{getCtx = push (CExcl [TAG_MAIN]) ctx}
                              frequencyState $ context2gens freqTbl tags $ push (CExcl [TAG_MAIN]) ctx
          lift $ return $ mconcat flow_


genTAG_main :: GenHtmlState
genTAG_main = debug "genTAG_main" $ do
  st <- get              
  let main_   = hasMain    st 
      ctx     = getCtx     st 
      depth   = getDepth   st
  -- put st{hasMain = True}
  if depth == 0
  then lift $ return $ toHtml "MAIN1"
  else do html_ <- put st{ hasMain = True 
                         , getCtx = push (CExcl [TAG_MAIN]) $ push CFlow ctx
                         , getDepth = iter depth
                         } >> arbFlowContent
          lift $ frequency [ (1, return $ toHtml "MAIN2")
                           , (4, return $ mainT html_)]


genTAG_aside :: GenHtmlState
genTAG_aside = debug "genTAG_aside" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "ASIDE1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_MAIN]) $ push CFlow ctx
                         , getDepth = iter depth} >> arbFlowContent
          lift $ frequency [ (1, return $ toHtml "ASIDE2")
                           , (4, return $ aside html_)]


genTAG_section :: GenHtmlState
genTAG_section = debug "genTAG_section" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "SECTION1" 
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbFlowContent 
          lift $ frequency [ (1, return $ toHtml "SECTION2")
                           , (4, return $ section html_)]


genTAG_nav :: GenHtmlState
genTAG_nav = debug "genTAG_nav" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "NAV1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_MAIN]) $ push CFlow ctx 
                         , getDepth = iter depth} >> arbFlowContent                   
          lift $ frequency [ (1, return $ toHtml "NAV2")
                           , (4, return $ section html_)]

genTAG_article :: GenHtmlState
genTAG_article = debug "genTAG_article" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "ARTICLE1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_MAIN]) $ push CFlow ctx 
                         , getDepth = iter depth} >> arbFlowContent
          lift $ frequency [ (1, return $ toHtml "ARTRICLE2")
                           , (4, return $ article html_)]

genTAG_a :: GenHtmlState
genTAG_a = debug "genTAG_a" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "A1"
  else do html_ <- put st{ getCtx = push (CExcl interactiveContent) ctx 
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "A2")
                           , (4, return $ a html_)]


genTAG_abbr :: GenHtmlState
genTAG_abbr = debug "genTAG_abbr" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "ABBR1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx 
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "ABBR2")
                           , (4, return $ abbr html_)]


genTAG_address :: GenHtmlState
genTAG_address = debug "genTAG_address" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "ADDRESS1"
  else do html_ <- put st{ getCtx = push (CExcl headingContent) 
                                    $ push (CExcl sectioningContent) 
                                    $ push (CExcl [TAG_HEADER, TAG_FOOTER,TAG_ADDRESS]) 
                                    $ push CFlow ctx 
                         , getDepth = iter depth} >> arbFlowContent
          lift $ frequency [ (1, return $ toHtml "ADDRESS2")
                           , (4, return $ address html_)]


genTAG_area :: GenHtmlState
genTAG_area = debug "genTAG_area" $ do
  st <- get              
  let ctx = getCtx st 
  lift $ return $ if ((findTagCtx TAG_MAP ctx) || (findTagCtx TAG_TEMPLATE ctx))
                  then area
                  else Empty


genTAG_audio :: GenHtmlState
genTAG_audio = debug "genTAG_audio" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "AUDIO"
  else do html_    <- put st{ getCtx = push (CExcl mediaElements) ctx 
                            , getDepth = iter depth} >> arbPhrasingContent
          tracks_  <- put st{getDepth = iter depth} >> (resizeState ndegree $ listOfState genTAG_track)
          sources_ <- put st{getDepth = iter depth} >> (resizeState ndegree $ listOfState  genTAG_source) 
          lift $ frequency [ (1, return $ audio $ (mconcat (sources_ ++ tracks_)) <> html_)
                           , (1, return $ (audio $ ((mconcat tracks_) <> html_)) ! src (toValue "."))]


genTAG_track :: GenHtmlState
genTAG_track = debug "genTAG_track" $ lift $ return $ track ! src (toValue ".")


genTAG_source :: GenHtmlState
genTAG_source = debug "genTAG_source" $ lift $ return $ source ! src (toValue ".")


genTAG_b :: GenHtmlState
genTAG_b = debug "genTAG_b" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "B1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent 
          lift $ frequency [ (1, return $ toHtml "B2")
                           , (4, return $ b html_)]


genTAG_bdi :: GenHtmlState
genTAG_bdi = debug "genTAG_bdi" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "BDI1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "BDI2")
                           , (4, return $ bdi html_)]


genTAG_bdo :: GenHtmlState
genTAG_bdo = debug "genTAG_bdo" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "BDO1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "BDO2")
                           , (4, return $ (bdo html_) ! dir (toValue "ltr"))]


genTAG_blockquote :: GenHtmlState
genTAG_blockquote = debug "genTAG_blockquote" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "BLOCKQUOTE1"
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbFlowContent
          lift $ frequency [ (1, return $ toHtml "BLOCKQUOTE2")
                           , (4, return $ blockquote html_)]


genTAG_br :: GenHtmlState
genTAG_br = lift $ return $ br


genTAG_button :: GenHtmlState
genTAG_button = debug "genTAG_button" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "BUTTON1"
  else do html_ <- put st{ getCtx = push (CExcl interactiveContent) $ push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "BUTTON2")
                           , (4, return $ (button html_))]


genTAG_canvas :: GenHtmlState
genTAG_canvas = debug "genTAG_canvas" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "CANVAS1"
  else do html_ <- put st{ getCtx = push (CExcl interactiveContent) ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "CANVAS2")
                           , (4, return $ canvas html_)]


genTAG_cite :: GenHtmlState
genTAG_cite = debug "genTAG_cite" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "CITE1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "CITE2")
                           , (4, return $ (H.cite html_))]


genTAG_code :: GenHtmlState
genTAG_code = debug "genTAG_code" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "CODE1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "CODE2")
                           , (4, return $ code html_)]


genTAG_data :: GenHtmlState
genTAG_data = debug "genTAG_data" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "DATA1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "DATA2")
                           , (4, return $ (dataT html_) ! value (toValue "10"))]


genTAG_date :: GenHtmlState
genTAG_date = undefined

genTAG_datalist :: GenHtmlState
genTAG_datalist = debug "genTAG_datalist" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "DATALIST1"
  else do phrase_  <- put st{ getCtx = push CPhrasing ctx
                            , getDepth = iter depth } >> arbPhrasingContent
          options  <- put st{ getDepth = iter depth } >> listOfState genTAG_option
          html_   <- lift $ frequency [ (1, return phrase_)
                                      , (1, return $ mconcat options)]
          lift $ frequency [ (1, return $ toHtml "DATA2")
                           , (4, return $ (datalist html_))]


genTAG_option :: GenHtmlState
genTAG_option = debug "genTAG_option" $ lift $ frequency 
                [ (1, return $ (option Empty) ! A.label (toValue "a") ! value (toValue "1"))
                , (1, return $ (option $ toHtml "OPTION1") ! A.label (toValue "a"))
                , (1, return $ (option $ toHtml "OPTION2"))
                ]


genTAG_del :: GenHtmlState
genTAG_del = debug "genTAG_del" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "DEL1"
  else do html_ <- put st{ getDepth = iter depth } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "DEL2")
                           , (4, return $ del html_)]


genTAG_details :: GenHtmlState
genTAG_details = debug "genTAG_details" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do sum_ <- put st{ getDepth = iter depth } >> genTAG_summary                  
          html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth } >> arbFlowContent
          lift $ frequency [ (1, return $ toHtml "DETAILS2")
                           , (4, return $ details $ sum_ <> html_)]


genTAG_summary :: GenHtmlState
genTAG_summary = debug "genTAG_summary" $ do
  st <- get                      
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
  if depth == 0
  then lift $ return $ H.summary Empty
  else do phr_ <- put st{ getCtx = push CPhrasing ctx
                        , getDepth = iter depth} >> arbPhrasingContent
          hea_ <- put st{ getCtx = ctx
                        , getDepth = iter depth} >> 
                  ( frequencyState 
                  $ context2gens freqTbl tags 
                  $ push CHeading ctx 
                  )
          lift $ frequency [ (1, return $ H.summary phr_)
                           , (1, return $ H.summary hea_)]


genTAG_dfn :: GenHtmlState
genTAG_dfn = debug "genTAG_dfn" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "DFN1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_DFN]) $ push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "DFN2")
                           , (4, return $ dfn html_)]


genTAG_dialog :: GenHtmlState
genTAG_dialog = debug "genTAG_dialog" $ do  
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "DIALOG1"
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "DIALOG2")
                           , (4, return $ dialog html_)]


genTAG_div :: GenHtmlState
genTAG_div = debug "genTAG_div" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "DIV1"
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "DIV2")
                           , (4, return $ H.div html_)]


genTAG_dl :: GenHtmlState
genTAG_dl = debug "genTAG_dl" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "DL1"
  else do let dt1 = put st{ getDepth = iter depth } >> genTAG_dt
              dd1 = put st{ getDepth = iter depth } >> genTAG_dd
              dts = put st{ getDepth = iter depth} >> 
                    ( resizeState ndegree 
                    $ listOfState 
                    $ oneofState [ genTAG_script
                                 , genTAG_template
                                 , genTAG_dt
                                 ]
                    )
              dds = put st{ getDepth = iter depth} >> 
                    ( resizeState ndegree
                    $ listOfState 
                    $ oneofState [ genTAG_script 
                                 , genTAG_template
                                 , genTAG_dd
                                 ]
                    )
          ss1 <- put st{ getDepth = iter depth} >>
                 ( resizeState ndegree
                 $ listOfState 
                 $ oneofState [genTAG_script
                              , genTAG_template
                              ]
                 )
          ss2 <- put st{ getDepth = iter depth} >>
                 ( resizeState ndegree
                 $ listOfState 
                 $ oneofState [ genTAG_script
                              , genTAG_template
                              ]
                 )
          ss3 <- put st{ getDepth = iter depth} >>
                 ( resizeState ndegree 
                 $ listOfState 
                 $ oneofState [ genTAG_script
                              , genTAG_template
                              ]
                 )
          html_ <- listOfState 
                   $ liftM4 (\dt1_ dd1_ dts_ dds_ -> 
                                 mconcat $ ( (dt1_:dts_) 
                                             ++ (dd1_:dds_)
                                           )
                            ) dt1 dd1 dts dds
          lift $ frequency [ (1, return $ toHtml "DL2")
                           , (4, return $ dl $ mconcat html_)]

genTAG_dd :: GenHtmlState
genTAG_dd = debug "genTAG_dd" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ dd Empty
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ return $ dd html_


genTAG_dt :: GenHtmlState
genTAG_dt = debug "genTAG_dt" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ dt Empty
  else do html_ <- put st{ getCtx = push (CExcl [TAG_HEADER, TAG_FOOTER]) $ 
                                    push (CExcl sectioningContent) $ 
                                    push (CExcl headingContent) $ 
                                    push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ return $ dt html_


genTAG_em :: GenHtmlState
genTAG_em = debug "genTAG_em" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "EM1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "EM2")
                           , (4, return $ em html_)]

genTAG_embed :: GenHtmlState
genTAG_embed = debug "genTAG_embed" $ lift $ return embed

               
genTAG_fieldset :: GenHtmlState
genTAG_fieldset = debug "genTAG_fieldset" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "FIELDSET1"
  else do leg_ <- put st{ getDepth = iter depth} >> optional genTAG_legend 
          html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ fieldset $ leg_ <> html_)]


genTAG_legend :: GenHtmlState
genTAG_legend = debug "genTAG_legend" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "LEGEND1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "LEGEND2")
                           , (4, return $ legend html_)]


genTAG_figure :: GenHtmlState
genTAG_figure = debug "genTAG_figure" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "FIGURE1"
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          cap_  <- put st{ getDepth = iter depth } >> genTAG_figcaption
          lift $ frequency [ (1, return $ figure $ cap_ <> html_)
                           , (1, return $ figure $ html_ <> cap_)
                           , (1, return $ figure html_)] 


genTAG_figcaption :: GenHtmlState
genTAG_figcaption = debug "genTAG_figcaption" $ do 
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "FIGCAPTION1"
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "FIGCAPTION2")
                           , (4, return $ figcaption html_)] 

genTAG_footer :: GenHtmlState
genTAG_footer = debug "genTAG_footer" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "FOOTER1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_HEADER, TAG_FOOTER, TAG_MAIN]) $ 
                                    push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "FOOTER2")
                           , (4, return $ footer html_)] 


genTAG_form :: GenHtmlState
genTAG_form = debug "genTAG_form" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "FORM1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_FORM]) $ 
                                    push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "FORM2")
                           , (4, return $ H.form html_)]


genTAG_h1 :: GenHtmlState
genTAG_h1 = debug "genTAG_h1" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ h1 html_)]


genTAG_h2 :: GenHtmlState
genTAG_h2 = debug "genTAG_h2" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ h2 html_)]

genTAG_h3 :: GenHtmlState
genTAG_h3 = debug "genTAG_h3" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ h3 html_)]

genTAG_h4 :: GenHtmlState
genTAG_h4 = debug "genTAG_h4" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ h4 html_)]

genTAG_h5 :: GenHtmlState
genTAG_h5 = debug "genTAG_h5" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ h5 html_)]

genTAG_h6 :: GenHtmlState
genTAG_h6 = debug "genTAG_h6" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ h6 html_)]


genTAG_header :: GenHtmlState
genTAG_header = debug "genTAG_header" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "HEADER1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_HEADER, TAG_FOOTER, TAG_MAIN]) $ 
                                    push CFlow ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "HEADER2")
                           , (4, return $ header html_)]

genTAG_hgroup :: GenHtmlState
genTAG_hgroup = debug "genTAG_hgroup" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return Empty
  else do hs_ <- put st{ getDepth = iter depth } >> 
                 ( liftM mconcat
                 $ resizeState ndegree
                 $ listOfState1 
                 $ frequencyState 
                 $ context2gens freqTbl tags 
                 $ push (CExcl [TAG_HGROUP]) 
                 $ push CHeading empty
                 )
          tmp_ <- put st{getDepth = iter depth} >> 
                  ( liftM mconcat 
                  $ resizeState ndegree
                  $ listOfState 
                  $ genTAG_template 
                  )
          if BI.null hs_ 
          then put st{getDepth = iter depth} >> genTAG_hgroup 
          else lift $ return $ hgroup $ hs_ <> tmp_


genTAG_hr :: GenHtmlState
genTAG_hr = debug "genTAG_hr" $ lift $ return hr
 

genTAG_i :: GenHtmlState
genTAG_i = debug "genTAG_i" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "I1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "I2")
                           , (4, return $ i html_)]

genTAG_iframe :: GenHtmlState
genTAG_iframe = debug "genTAG_iframe" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "IFRAME1"
  else do --html_ <- arbHtmlHTML cm $ iter n 
          -- html_ <- arbPhrasingContent cm $ iter n
          html_ <- return $ toHtml "IFRAME3"
          lift $ frequency [ (1, return $ toHtml "IFRAME2")
                           , (4, return $ iframe html_)]


genTAG_img :: GenHtmlState
genTAG_img = debug "genTAG_img" $ lift $ return $ img ! src (toValue "src") ! alt (toValue "alt")

genTAG_input :: GenHtmlState
genTAG_input = debug "genTAG_input" $ lift $ return input 


genTAG_ins :: GenHtmlState
genTAG_ins = debug "genTAG_ins" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "INS1"
  else do html_ <- put st{ getDepth = iter depth } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "INS2")
                           , (4, return $ ins html_)]


genTAG_kbd :: GenHtmlState
genTAG_kbd = debug "genTAG_kbd" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "KBD1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "KBD2")
                           , (4, return $ kbd html_)]


genTAG_keygen :: GenHtmlState
genTAG_keygen = debug "genTAG_keygen" $ lift $ return keygen


genTAG_label :: GenHtmlState
genTAG_label = debug "genTAG_label" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "LABEL1"
  else do html_ <- put st{ getCtx = push (CExcl labelableElements) $ 
                                    push (CExcl [TAG_LABEL]) $ 
                                    push CPhrasing ctx
                         , getDepth = iter depth 
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "LABEL2")
                           , (4, return $ H.label html_)]

genTAG_li :: GenHtmlState
genTAG_li = debug "genTAG_li" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getCtx = push CFlow ctx
                         , getDepth = iter depth 
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ li html_)]

genTAG_map :: GenHtmlState
genTAG_map = debug "genTAG_map" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
  if depth == 0
  then lift $ return $ toHtml "MAP1"
  else do html_ <- put st{ getCtx = push (CIncl [TAG_MAP]) ctx
                         , getDepth = iter depth 
                         } >> 
                   ( frequencyState
                   $ context2gens freqTbl tags 
                   $ push (CIncl [TAG_MAP]) ctx
                   )
          lift $ frequency [ (1, return $ toHtml "MAP2")
                           , (4, return $ (H.map html_) ! name (toValue "name"))]


genTAG_mark :: GenHtmlState
genTAG_mark = debug "genTAG_mark" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "MARK1"
  else do html_ <- put st{ getCtx = push CPhrasing ctx
                         , getDepth = iter depth 
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "MARK2")
                           , (4, return $ mark html_)]


genTAG_math :: GenHtmlState
genTAG_math = debug "genTAG_math" $ lift $ return $ math Empty



genTAG_menu :: GenHtmlState
genTAG_menu = debug "genTAG_menu" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "MENU1"
  else do flow_ <- put st{ getCtx = push CPalpable ctx
                         , getDepth = iter depth 
                         } >> arbPhrasingContent
          lis_  <- put st{ getCtx = push CPalpable ctx
                         , getDepth = iter depth 
                         } >> (resizeState ndegree $ listOfState $ genTAG_li) 
          scrp_ <- put st{ getCtx = push CPalpable ctx
                         , getDepth = iter depth 
                         } >> 
                   ( resizeState ndegree
                   $ listOfState 
                   $ frequencyState 
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [TAG_SCRIPT, TAG_TEMPLATE]) empty
                   )
          toolbar_ <- lift $ frequency [ (1, return flow_)
                                       , (1, return $ mconcat lis_)
                                       , (1, return $ mconcat scrp_)]
          popup_ <- put st{ getCtx = push CFlow ctx -- TODO: temporaraly excluded TAG_menu_popup
                         , getDepth = iter depth 
                         } >> 
                    ( resizeState ndegree
                    $ listOfState 
                    $ frequencyState 
                    $ context2gens freqTbl tags
                    $ push (CArbitr [TAG_MENUITEM, TAG_HR, TAG_SCRIPT, TAG_TEMPLATE]) empty 
                    )
          lift $ frequency [ (1, return $ toHtml "MENU2")
                           -- , (4, return $ (menu toolbar_) ! type_ (toValue "toolbar"))
                           , (4, return $ (menu $ mconcat popup_) ! type_ (toValue "popup"))]

                                          
genTAG_menuitem :: GenHtmlState
genTAG_menuitem = debug "genTAG_menuitem" $ lift $ return $ menuitemT Empty


genTAG_menu_popup :: GenHtmlState
genTAG_menu_popup = debug "genTAG_menu_popup" $ do 
  html_ <- genTAG_menu_popup
  lift $ return (html_ ! type_ (toValue "popup"))        


genTAG_meter :: GenHtmlState
genTAG_meter = debug "genTAG_meter" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "METER1"
  else do html_ <- put st{ getCtx = push (CExcl [TAG_METER]) $ push CPhrasing ctx
                         , getDepth = iter depth 
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "METER2")
                           , (4, return $ meter html_ ! value (toValue "1"))]


genTAG_object :: GenHtmlState
genTAG_object = debug "genTAG_object" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "OBJECT1"
  else do html_ <- put st{ getDepth = iter depth } >> arbPhrasingContent
          params_ <- put st{ getDepth = iter depth } >> (resizeState ndegree $ listOfState $ genTAG_param)
          lift $ frequency [ (1, return $ toHtml "OBJECT2")
                           , (4, return $ object (mconcat params_ <> html_) ! data_ (toValue "."))]
        

genTAG_param :: GenHtmlState
genTAG_param = debug "genTAG_param" $ lift $ return $ param ! name (toValue "name") ! value (toValue "value")


genTAG_ol :: GenHtmlState
genTAG_ol = debug "genTAG_ol" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "OL1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CFlow ctx} >> 
                   ( resizeState ndegree
                   $ listOfState 
                   $ frequencyState 
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [TAG_LI, TAG_SCRIPT, TAG_TEMPLATE]) empty
                   )
          lift $ frequency [ (1, return $ toHtml "OL2")
                           , (4, return $ ol $ mconcat html_)]


genTAG_optgroup :: GenHtmlState
genTAG_optgroup = debug "genTAG_optgroup" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getDepth = iter depth } >> 
                   ( resizeState ndegree
                   $ listOfState 
                   $ frequencyState 
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [TAG_OPTION, TAG_SCRIPT, TAG_TEMPLATE]) empty
                   )
          lift $ return $ flip (!) (A.label $ toValue "a") $ optgroup $ mconcat html_


genTAG_output :: GenHtmlState
genTAG_output = debug "genTAG_output" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "OUTPUT1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "OUTPUT2")
                           , (4, return $ output html_)]

genTAG_p :: GenHtmlState
genTAG_p = debug "genTAG_p" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "P1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "P2")
                           , (4, return $ p html_)]


genTAG_pre :: GenHtmlState
genTAG_pre = debug "genTAG_pre" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "PRE1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "PRE2")
                           , (4, return $ pre html_)]


genTAG_progress :: GenHtmlState
genTAG_progress = debug "genTAG_progress" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "PROGRESS1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push (CExcl [TAG_PROGRESS]) $ push CPhrasing ctx} >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "PROGRESS2")
                           , (4, return $ progress html_)]


genTAG_q :: GenHtmlState
genTAG_q = debug "genTAG_q" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "Q1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "Q2")
                           , (4, return $ q html_)]

genTAG_rp :: GenHtmlState
genTAG_rp = debug "genTAG_rp" $ lift $ return $ rp $ toHtml "RP"


genTAG_rt :: GenHtmlState
genTAG_rt = debug "genTAG_rt" $ lift $ return $ rt $ toHtml "RT"


genTAG_ruby :: GenHtmlState
genTAG_ruby = debug "genTAG_ruby" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "RUBY1"
  else do html1_ <- put st{ getDepth = iter depth } >> 
                    (liftM mconcat $ resizeState ndegree $ listOfState1 $ genTAG_rt)
          rp1 <- put st{ getDepth = iter depth } >> genTAG_rp
          rt  <- put st{ getDepth = iter depth } >> genTAG_rt
          rp2 <- put st{ getDepth = iter depth } >> genTAG_rp
          html2_ <- liftM mconcat $ resizeState ndegree $ listOfState1 $ return (rp1 <> rt <> rp2)
          lift $ frequency [ (1, return $ toHtml "RUBY2")
                           , (4, return $ ruby html1_)
                           , (4, return $ ruby html2_)]


genTAG_s :: GenHtmlState
genTAG_s = debug "genTAG_s" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "S1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "S2")
                           , (4, return $ s html_)]


genTAG_samp :: GenHtmlState
genTAG_samp = debug "genTAG_samp" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "SAMP1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "SAMP2")
                           , (4, return $ samp html_)]


genTAG_select :: GenHtmlState
genTAG_select = debug "genTAG_select" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getDepth = iter depth 
                         } >> 
                   ( resizeState ndegree
                   $ listOfState 
                   $ frequencyState 
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [TAG_OPTION, TAG_OPTGROUP, TAG_TEMPLATE]) empty
                   )
          lift $ return $ select $ mconcat html_


genTAG_small :: GenHtmlState
genTAG_small = debug "genTAG_small" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "SMALL1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "SMALL2")
                           , (4, return $ small html_)]


genTAG_span :: GenHtmlState
genTAG_span = debug "genTAG_span" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "SPAN1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "SPAN2")
                           , (4, return $ H.span html_)]


genTAG_strong :: GenHtmlState
genTAG_strong = debug "genTAG_strong" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "STRONG1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "STRONG2")
                           , (4, return $ strong html_)]


genTAG_sub :: GenHtmlState
genTAG_sub = debug "genTAG_sub" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "SUB1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "SUB2")
                           , (4, return $ sub html_)]


genTAG_sup :: GenHtmlState
genTAG_sup = debug "genTAG_sup" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "SUP1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "SUP2")
                           , (4, return $ sup html_)]

genTAG_svg :: GenHtmlState
genTAG_svg = debug "genTAG_svg" $ lift $ return $ svg Empty


genTAG_table :: GenHtmlState
genTAG_table = debug "genTAG_table" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "TABLE1"
  else do cap_    <- put st{ getDepth = iter depth } >> (optional $ genTAG_caption)
          colgr_  <- put st{ getDepth = iter depth } >> (resizeState ndegree $ listOfState $ genTAG_colgroup )
          thead_  <- put st{ getDepth = iter depth } >> (optional $ genTAG_thead)
          tfoot1_ <- put st{ getDepth = iter depth } >> (optional $ genTAG_tfoot)
          tbody_  <- put st{ getDepth = iter depth } >> (resizeState ndegree $ listOfState $ genTAG_tbody)
          tr_     <- put st{ getDepth = iter depth } >> (resizeState ndegree $ listOfState1 $ genTAG_tr)
          tfoot2_ <- put st{ getDepth = iter depth } >> (optional $ genTAG_tfoot)
          let tfoot3_ = if BI.null tfoot1_ then tfoot2_ else Empty
          lift $ frequency [ (1, return $ toHtml "TABLE2")
                           , (4, return $ table $ mconcat 
                                   $ [cap_] 
                              ++ colgr_ 
                              ++ [thead_] 
                              ++ [tfoot1_] 
                              ++ tbody_ 
                              ++ tr_ 
                              ++ [tfoot3_])]


genTAG_caption :: GenHtmlState
genTAG_caption = debug "genTAG_caption" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push (CExcl [TAG_TABLE]) $ push CFlow ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return Empty)
                           , (4, return $ caption html_)]


genTAG_colgroup :: GenHtmlState
genTAG_colgroup = debug "genTAG_colgroup" $ lift $ return Empty
-- genTAG_colgroup _  0 = lift $ return Empty
-- genTAG_colgroup cm n = do html_ <- resizeState 3 $ listOfState 
--                                    $ oneofState $ mkGens cm (iter n)
--                                          [ genTAG_col
--                                          , genTAG_template]
--                           lift $ frequency [ (1, return Empty)
--                                            , (4, return $ (colgroup Empty) ! A.span (toValue "1"))
--                                            , (4, return $ (colgroup $ mconcat html_) )]

genTAG_thead :: GenHtmlState
genTAG_thead = debug "genTAG_thead" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getDepth = iter depth } >> 
                   ( resizeState ndegree
                   $ listOfState 
                   $ frequencyState
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [ TAG_TR, TAG_TEMPLATE, TAG_SCRIPT]) empty)
          lift $ frequency [ (1, return Empty)
                           , (4, return $ thead $ mconcat html_)]


genTAG_tfoot :: GenHtmlState
genTAG_tfoot = debug "genTAG_tfoot" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getDepth = iter depth } >> 
                   ( resizeState ndegree
                   $ listOfState 
                   $ frequencyState
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [ TAG_TR, TAG_TEMPLATE, TAG_SCRIPT]) empty)
          lift $ frequency [ (1, return Empty)
                           , (4, return $ tfoot $ mconcat html_)]


genTAG_tbody :: GenHtmlState
genTAG_tbody = debug "genTAG_tbody" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getDepth = iter depth } >> 
                   ( resizeState ndegree
                   $ listOfState 
                   $ frequencyState
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [ TAG_TR, TAG_TEMPLATE, TAG_SCRIPT]) empty)
          lift $ frequency [ (1, return Empty)
                           , (4, return $ tbody $ mconcat html_)]


genTAG_tr :: GenHtmlState
genTAG_tr = debug "genTAG_tr" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return Empty
  else do html_ <- put st{ getDepth = iter depth } >> 
                   ( resizeState ndegree
                   $ listOfState1 
                   $ frequencyState
                   $ context2gens freqTbl tags
                   $ push (CArbitr [ TAG_TD, TAG_TH]) empty) 
          -- TODO: TAG_TEMPLATE, TAG_SCRIPT
          lift $ frequency [ (1, return Empty)
                           , (4, return $ tr $ mconcat html_)]


genTAG_td :: GenHtmlState
genTAG_td = debug "genTAG_td" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then  lift $ return $ td $ toHtml "TD1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CFlow ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ td $ toHtml "TD2")
                           , (4, return $ td html_)]


genTAG_th :: GenHtmlState
genTAG_th = debug "genTAG_th" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then  lift $ return $ th $ toHtml "TH1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push (CExcl $ [TAG_HEADER, TAG_FOOTER]
                                          ++ sectioningContent 
                                          ++ headingContent) $ push CFlow ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ th $ toHtml "TH1")
                           , (4, return $ th html_)]


genTAG_textarea :: GenHtmlState
genTAG_textarea = debug "genTAG_textarea" $ lift $ return $ textarea $ toHtml "TEXTAREA"
 

genTAG_time :: GenHtmlState
genTAG_time = debug "genTAG_time" $ lift $ return $ time $ toHtml "10:00"
-- genTAG_time _  0 = lift $ return $ toHtml "TIME1"
-- genTAG_time cm n = do html_ <- arbPhrasingContent (push (Right CPhrasing) cm) $ iter n
--                       lift $ frequency [ (1, return $ toHtml "TIME2")
--                                        , (4, return $ time html_)]


genTAG_u :: GenHtmlState
genTAG_u = debug "genTAG_u" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then  lift $ return $ toHtml "U1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "U2")
                           , (4, return $ u html_)]


genTAG_ul :: GenHtmlState
genTAG_ul = debug "genTAG_ul" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      freqTbl = tagFreqTbl st
      tags    = htmlTags   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "UL1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CFlow ctx
                         } >> 
                   ( resizeState ndegree
                   $ listOfState1 
                   $ frequencyState 
                   $ context2gens freqTbl tags 
                   $ push (CArbitr [ TAG_LI, TAG_SCRIPT, TAG_TEMPLATE]) empty
                   )
          lift $ frequency [ (1, return $ toHtml "UL2")
                           , (4, return $ ol $ mconcat html_)]


genTAG_var :: GenHtmlState
genTAG_var = debug "genTAG_var" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
  if depth == 0
  then lift $ return $ toHtml "VAR1"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx = push CPhrasing ctx
                         } >> arbPhrasingContent
          lift $ frequency [ (1, return $ toHtml "VAR2")
                           , (4, return $ var html_)]


genTAG_video :: GenHtmlState
genTAG_video = debug "genTAG_video" $ do
  st <- get              
  let ctx     = getCtx     st 
      depth   = getDepth   st
      ndegree = getNodeDegr st 
  if depth == 0
  then lift $ return $ toHtml "VIDEO"
  else do html_ <- put st{ getDepth = iter depth 
                         , getCtx   = push (CExcl mediaElements) ctx
                         } >> arbPhrasingContent
          tracks_ <- put st{ getDepth = iter depth } >> 
                     ( resizeState ndegree
                     $ listOfState 
                     $ genTAG_track)
          sources_ <- put st{ getDepth = iter depth } >> 
                      ( resizeState ndegree
                      $ listOfState 
                      $ genTAG_source) 
          lift $ frequency [ (1, return $ video $ (mconcat (sources_ ++ tracks_)) <> html_)
                           , (1, return $ (video $ ((mconcat tracks_) <> html_)) ! src (toValue "."))]


genTAG_wbr :: GenHtmlState
genTAG_wbr = debug "genTAG_wbr" $ lift $ return wbr

