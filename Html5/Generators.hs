module Html5.Generators where

import Prelude
import qualified Prelude as P

import Control.Monad
import Data.Monoid
import Data.List
import Data.Either
import Util.Stack
import Util.Iterate
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Html5.Context
import Html5.Tags

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

import Html5.QuickCheck.Gen

import Util.Debug


fromTag2Gen :: HTML_TAG -> (Context -> Int -> GenState HasMain Html)
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
fromTag2Gen TAG_COL        = genTAG_col
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


context2gens :: Context -> [Context -> Int -> GenState HasMain Html]
context2gens ctx = gens
    where
      ctxList = stack2list ctx
      negCtx  = P.map cmodel2tags $ filter isCExclude ctxList  
      posCtx  = P.map cmodel2tags $ filter isContext ctxList
      passCtx = P.map cmodel2tags $ filter isCInclude ctxList
      ctxUni  = (foldl (\\) (foldr1 intersect posCtx) negCtx) ++ concat passCtx
      gens    = P.map fromTag2Gen  ctxUni



arbHtmlHTML :: Context -> Int -> GenState HasMain Html
arbHtmlHTML _ 0  = lift $ return $ toHtml "HTML"
arbHtmlHTML cm n = debug "arbHtmlHTML" $
                   do head_ <- arbHtmlHEAD (push CMetadata cm) $ iter n 
                      body_ <- arbHtmlBODY (push CFlow cm) $ iter n
                      lift $ return $ docTypeHtml $ head_ >> body_
             

arbHtmlHEAD :: Context -> Int -> GenState HasMain Html
arbHtmlHEAD _ 0  = lift $ return $ toHtml "head"
arbHtmlHEAD cm n = debug "arbHtmlHEAD" $
                   do metadata_ <- listOfState $ arbHtmlMETADATA cm $ iter n
                      base_     <- optional $ genTAG_base cm n
                      title_    <- genTAG_title cm n
                      lift $ return $ H.head $ title_ >> base_ >> (mconcat metadata_)


arbHtmlMETADATA :: Context -> Int -> GenState HasMain Html
arbHtmlMETADATA _ 0 = lift $ return Empty
arbHtmlMETADATA cm n = debug "arbHtmlMETADATA" $ 
                       do meta_ <- sequence [ genTAG_link cm n
                                            , genTAG_style cm n
                                            , genTAG_meta cm n
                                            , genTAG_noscript cm n
                                            , genTAG_script cm n]
                          lift $ elements meta_


genTAG_link :: Context -> Int -> GenState HasMain Html
genTAG_link cm _ = debug "genTAG_link" $
                   case getFstCtx cm of 
                     Just CMetadata ->  lift $ return $ link ! rel (toValue "alternate") ! href (toValue ".")
                     Just c | c == CPhrasing || c == CFlow 
                                ->  lift $ return $ link ! href (toValue "href") !  itemprop (toValue "url") 
                            | otherwise -> error $ "*** genTAG_link " ++ show c
                     Nothing        -> error $ "genTAG_link: there is no right context"

genTAG_style :: Context -> Int -> GenState HasMain Html
genTAG_style _ _ = debug "genTAG_style" $
                   lift$ return $ H.style Empty

genTAG_base :: Context -> Int -> GenState HasMain Html
genTAG_base _ _ = debug "genTAG_base" $
                  lift $ return $ base ! href (toValue ".") ! target (toValue "_blank")

genTAG_meta :: Context -> Int -> GenState HasMain Html
genTAG_meta cm _ = debug "genTAG_meta" $
                   case getFstCtx cm of
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



genTAG_noscript :: Context -> Int -> GenState HasMain Html
genTAG_noscript _ 0  = lift $ return $ toHtml "NOSCRIPT1"
genTAG_noscript cm n = debug "genTAG_noscript" $
                       case getFstCtx cm of
                         Just CMetadata -> lift $ return $ noscript Empty
                         Just c | c == CPhrasing || c == CFlow -> 
                                    do html_ <- arbPhrasingContent (push (CExcl [TAG_NOSCRIPT]) cm) $ iter n
                                       lift $ frequency [ (1, return $ toHtml "NOSCRIPT2")
                                                        , (4, return $ noscript html_)]
                                | otherwise -> error $ "*** genTAG_noscript " ++ show c
                         Nothing        -> error $ "genTAG_noscript: there is no right context"
  

genTAG_script :: Context -> Int -> GenState HasMain Html
genTAG_script _ _  = debug "genTAG_script" $
                     lift $ return $ script $ toHtml "SCRIPT"


genTAG_title :: Context -> Int -> GenState HasMain Html
genTAG_title _ _ = debug "genTAG_title" $
                   lift $ return $ H.title $ toHtml "Title"


arbHtmlBODY :: Context -> Int -> GenState HasMain Html
arbHtmlBODY _ 0 = lift $ return $ toHtml "BODY"
arbHtmlBODY cm n = debug "arbHtmlBODY" $
                   do html_ <- arbFlowContent (push CFlow cm) $ iter n 
                      lift $ return $ (body html_) ! itemscope (toValue "") ! itemtype (toValue "http://schema.org/WebPage")


arbFlowContent :: Context -> Int -> GenState HasMain Html
arbFlowContent _  0  = lift $ return $ toHtml $ "FLOW CONTENT"
arbFlowContent cm n = debug "arbFlowContent" $
                      do flow_ <- resizeState 3 $ listOfState $ 
                                  do st <- get
                                     if st 
                                     -- then oneofState $ mkGens cm n gensFC 
                                     then oneofState $ mkGens cm n $ context2gens cm
                                     -- else oneofState $ mkGens cm n genFCNoMain     
                                     else oneofState $ mkGens cm n $ context2gens $ push (CExcl [TAG_MAIN]) cm
                         lift $ return $ mconcat flow_


arbPhrasingContent :: Context -> Int -> GenState HasMain Html
arbPhrasingContent _  0 = lift $ return $ toHtml $ "PHRASING CONTENT"
arbPhrasingContent cm n = debug "arbPhrasingContent" $
                          do flow_ <- resizeState 3 $ listOfState $ 
                                      do st <- get
                                         if st 
                                         then oneofState $ mkGens cm n $ context2gens cm
                                         else oneofState $ mkGens cm n $ context2gens $ push (CExcl [TAG_MAIN]) cm
                             lift $ return $ mconcat flow_


genTAG_main :: Context -> Int -> GenState HasMain Html
genTAG_main _ 0 = lift $ return $ toHtml "MAIN1"
genTAG_main cm n = debug "genTAG_main" $
                   do put False
                      html_ <- arbFlowContent ( push (CExcl [TAG_MAIN]) 
                                              $ push CFlow cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "MAIN2")
                                       , (4, return $ mainT html_)]


genTAG_aside :: Context -> Int -> GenState HasMain Html
genTAG_aside _  0 = lift $ return $ toHtml "ASIDE1"
genTAG_aside cm n = debug "genTAG_aside" $
                    do html_ <- arbFlowContent ( push (CExcl [TAG_MAIN]) 
                                               $ push CFlow cm) $ iter n
                       lift $ frequency [ (1, return $ toHtml "ASIDE2")
                                        , (4, return $ aside html_)]


genTAG_section :: Context -> Int -> GenState HasMain Html
genTAG_section _  0 = lift $ return $ toHtml "SECTION1"
genTAG_section cm n = debug "genTAG_section" $
                      do html_ <- arbFlowContent (push CFlow cm) $ iter n
                         lift $ frequency [ (1, return $ toHtml "SECTION2")
                                          , (4, return $ section html_)]


genTAG_nav :: Context -> Int -> GenState HasMain Html
genTAG_nav _  0 = lift $ return $ toHtml "NAV1"
genTAG_nav cm n = debug "genTAG_nav" $
                  do html_ <- arbFlowContent ( push (CExcl [TAG_MAIN]) 
                                             $ push CFlow cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "NAV2")
                                      , (4, return $ section html_)]


genTAG_article :: Context -> Int -> GenState HasMain Html
genTAG_article _  0 = lift $ return $ toHtml "ARTICLE1"
genTAG_article cm n = debug "genTAG_article" $
                      do html_ <- arbFlowContent ( push (CExcl [TAG_MAIN]) 
                                                 $ push CFlow cm) $ iter n
                         lift $ frequency [ (1, return $ toHtml "ARTRICLE2")
                                          , (4, return $ article html_)]



genTAG_a :: Context -> Int -> GenState HasMain Html
genTAG_a _  0 = lift $ return $ toHtml "A1"
genTAG_a cm n = debug "genTAG_a" $
                do html_ <- arbPhrasingContent ( push (CExcl interactiveContent) cm) $ iter n
                   lift $ frequency [ (1, return $ toHtml "A2")
                                    , (4, return $ a html_)]


genTAG_abbr :: Context -> Int -> GenState HasMain Html
genTAG_abbr _  0 = lift $ return $ toHtml "ABBR1"
genTAG_abbr cm n = debug "genTAG_abbr" $
                   do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "ABBR2")
                                       , (4, return $ abbr html_)]


genTAG_address :: Context -> Int -> GenState HasMain Html
genTAG_address _ 0 = lift $ return $ toHtml "ADDRESS1"
genTAG_address cm n = debug "genTAG_address" $
                      do html_ <- arbFlowContent ( push (CExcl headingContent) 
                                                 $ push (CExcl sectioningContent)
                                                 $ push (CExcl [TAG_HEADER, TAG_FOOTER,TAG_ADDRESS])
                                                 $ push CFlow cm) $ iter n
                         lift $ frequency [ (1, return $ toHtml "ADDRESS2")
                                          , (4, return $ address html_)]


genTAG_area :: Context -> Int -> GenState HasMain Html
genTAG_area cm _ = debug "genTAG_area" $
                   lift $ return $ if ( (findTagCtx TAG_MAP cm) 
                                        || (findTagCtx TAG_TEMPLATE cm))
                                   then area
                                   else Empty


genTAG_audio :: Context -> Int -> GenState HasMain Html
genTAG_audio _  0 = lift $ return $ toHtml "AUDIO"
genTAG_audio cm n = debug "genTAG_audio" $
                    do html_    <- arbPhrasingContent (push (CExcl mediaElements) cm) $ iter n
                       tracks_  <- resizeState 3 $ listOfState $ genTAG_track cm $ iter n
                       sources_ <- resizeState 3 $ listOfState $ genTAG_source cm $ iter n 
                       lift $ frequency [ (1, return $ audio $ (mconcat (sources_ ++ tracks_)) <> html_)
                                        , (1, return $ (audio $ ((mconcat tracks_) <> html_)) ! src (toValue "."))]

genTAG_b :: Context -> Int -> GenState HasMain Html
genTAG_b _  0 = lift $ return $ toHtml "B1"
genTAG_b cm n = debug "genTAG_b" $
                do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                   lift $ frequency [ (1, return $ toHtml "B2")
                                    , (4, return $ b html_)]


genTAG_bdi :: Context -> Int -> GenState HasMain Html
genTAG_bdi _  0 = lift $ return $ toHtml "BDI1"
genTAG_bdi cm n = debug "genTAG_bdi" $
                  do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "BDI2")
                                      , (4, return $ bdi html_)]

genTAG_bdo :: Context -> Int -> GenState HasMain Html
genTAG_bdo _  0 = lift $ return $ toHtml "BDO1"
genTAG_bdo cm n = debug "genTAG_bdo" $
                  do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "BDO2")
                                      , (4, return $ (bdo html_) ! dir (toValue "ltr"))]


genTAG_blockquote :: Context -> Int -> GenState HasMain Html
genTAG_blockquote _  0 = lift $ return $ toHtml "BLOCKQUOTE1"
genTAG_blockquote cm n = debug "genTAG_blockquote" $
                         do html_ <- arbFlowContent (push CFlow cm) $ iter n
                            lift $ frequency [ (1, return $ toHtml "BLOCKQUOTE2")
                                             , (4, return $ blockquote html_)]

genTAG_br :: Context -> Int -> GenState HasMain Html
genTAG_br _ _ = lift $ return $ br

genTAG_button :: Context -> Int -> GenState HasMain Html
genTAG_button _  0 = lift $ return $ toHtml "BUTTON1"
genTAG_button cm n = debug "genTAG_button" $
                     do html_ <- arbPhrasingContent ( push (CExcl interactiveContent) 
                                                    $ push CPhrasing cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "BUTTON2")
                                         , (4, return $ (button html_))]


genTAG_canvas :: Context -> Int -> GenState HasMain Html
genTAG_canvas _ 0  = lift $ return $ toHtml "CANVAS1"
genTAG_canvas cm n = debug "genTAG_canvas" $
                     do html_ <- arbPhrasingContent (push (CExcl interactiveContent) cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "CANVAS2")
                                         , (4, return $ canvas html_)]
 

genTAG_cite :: Context -> Int -> GenState HasMain Html
genTAG_cite _ 0  = lift $ return $ toHtml "CITE1"
genTAG_cite cm n = debug "genTAG_cite" $
                   do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "CITE2")
                                       , (4, return $ (H.cite html_))]
 
genTAG_code :: Context -> Int -> GenState HasMain Html
genTAG_code _ 0  = lift $ return $ toHtml "CODE1"
genTAG_code cm n = debug "genTAG_code" $
                   do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "CODE2")
                                       , (4, return $ code html_)]

 
genTAG_data :: Context -> Int -> GenState HasMain Html
genTAG_data _ 0  = lift $ return $ toHtml "DATA1"
genTAG_data cm n = debug "genTAG_data" $
                   do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "CODE2")
                                       , (4, return $ (dataT html_) ! value (toValue "10"))]

 
genTAG_date :: Context -> Int -> GenState HasMain Html
genTAG_date = undefined
 
genTAG_datalist :: Context -> Int -> GenState HasMain Html
genTAG_datalist _ 0  = lift $ return $ toHtml "DATALIST1"
genTAG_datalist cm n = debug "genTAG_datalist" $
                       do phrase_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                          options <- listOfState $ genTAG_option cm $ iter n
                          html_   <- lift $ frequency [ (1, return phrase_)
                                                      , (1, return $ mconcat options)]
                          lift $ frequency [ (1, return $ toHtml "DATALIST2")
                                           , (4, return $ datalist html_)]
 
genTAG_del :: Context -> Int -> GenState HasMain Html
genTAG_del _ 0  = lift $ return $ toHtml "DEL1"
genTAG_del cm n = debug "genTAG_del" $
                  do html_ <- arbPhrasingContent cm $ iter n
                     lift $ frequency [ (1, return $ toHtml "DEL2")
                                      , (4, return $ del html_)]
 
genTAG_details :: Context -> Int -> GenState HasMain Html
genTAG_details _ 0 = lift $ return Empty
genTAG_details cm n = debug "genTAG_details" $
                      do sum_  <- genTAG_summary cm $ iter n
                         html_ <- arbFlowContent (push CFlow cm) $ iter n
                         lift $ frequency [ (1, return $ toHtml "DETAILS2")
                                          , (4, return $ details $ sum_ <> html_)]
 
genTAG_dfn :: Context -> Int -> GenState HasMain Html
genTAG_dfn _ 0  = lift $ return $ toHtml "DFN1"
genTAG_dfn cm n = debug "genTAG_dfn" $
                  do html_ <- arbPhrasingContent ( push (CExcl [TAG_DFN]) 
                                                 $ push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "DFN2")
                                      , (4, return $ dfn html_)]
 

genTAG_dialog :: Context -> Int -> GenState HasMain Html
genTAG_dialog _  0 = lift $ return $ toHtml "DIALOG1"
genTAG_dialog cm n = debug "genTAG_dialog" $ 
                     do html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "DIALOG2")
                                         , (4, return $ dialog html_)]
 

genTAG_div :: Context -> Int -> GenState HasMain Html
genTAG_div _  0 = lift $ return $ toHtml "DIV1"
genTAG_div cm n = debug "genTAG_div" $
                  do html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "DIV2")
                                      , (4, return $ H.div html_)]
 

                    
genTAG_dl :: Context -> Int -> GenState HasMain Html
genTAG_dl _  0 = lift $ return $ toHtml "DL1"
genTAG_dl cm n = debug "genTAG_dl" $
                 do let dt1 = genTAG_dt cm $ iter n
                        dd1 = genTAG_dd cm $ iter n
                        dts = resizeState 3 $ listOfState 
                              $ oneofState $ mkGens cm (iter n) 
                                    [ genTAG_script
                                    , genTAG_template
                                    , genTAG_dt]
                        dds = resizeState 3 $ listOfState 
                              $ oneofState $ mkGens cm (iter n) 
                                    [ genTAG_script 
                                    , genTAG_template
                                    , genTAG_dd]
                    ss1 <- resizeState 3 $ listOfState 
                           $ oneofState $ mkGens cm (iter n) 
                                 [genTAG_script
                                 , genTAG_template]
                    ss2 <- resizeState 3 $ listOfState 
                           $ oneofState $ mkGens cm (iter n) 
                                 [genTAG_script
                                 , genTAG_template]
                    ss3 <- resizeState 3 $ listOfState 
                           $ oneofState $ mkGens cm (iter n) 
                                 [genTAG_script
                                 , genTAG_template]
                    html_ <- listOfState $ liftM4 (\dt1_ dd1_ dts_ dds_ -> mconcat $ ((dt1_:dts_) ++ (dd1_:dds_))) dt1 dd1 dts dds
                    lift $ frequency [ (1, return $ toHtml "DL2")
                                     , (4, return $ dl $ mconcat html_)]
                    


genTAG_dt :: Context -> Int -> GenState HasMain Html
genTAG_dt _  0 = lift $ return $ dt Empty
genTAG_dt cm n = debug "genTAG_dt" $
                 do html_ <- arbPhrasingContent ( push (CExcl [TAG_HEADER, TAG_FOOTER]) 
                                                $ push (CExcl sectioningContent)
                                                $ push (CExcl headingContent)
                                                $ push CFlow cm) $ iter n
                    lift $ return $ dt html_


genTAG_dd :: Context -> Int -> GenState HasMain Html
genTAG_dd _  0 = lift $ return $ dd Empty
genTAG_dd cm n = debug "genTAG_dd" $
                 do html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                    lift $ return $ dd html_

 
genTAG_em :: Context -> Int -> GenState HasMain Html
genTAG_em _  0 = lift $ return $ toHtml "EM1"
genTAG_em cm n = debug "genTAG_em" $
                 do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                    lift $ frequency [ (1, return $ toHtml "EM2")
                                     , (4, return $ em html_)]

 
genTAG_embed :: Context -> Int -> GenState HasMain Html
genTAG_embed _ _ = debug "genTAG_embed" $
                   lift $ return embed


genTAG_fieldset :: Context -> Int -> GenState HasMain Html
genTAG_fieldset _  0 = lift $ return $ toHtml "FIELDSET1"
genTAG_fieldset cm n = debug "genTAG_fieldset" $
                       do leg_ <- optional $ genTAG_legend cm $ iter n
                          html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                          lift $ frequency [ (1, return Empty)
                                           , (4, return $ fieldset $ leg_ <> html_)]

genTAG_figure :: Context -> Int -> GenState HasMain Html
genTAG_figure _ 0  = lift $ return $ toHtml "FIGURE1"
genTAG_figure cm n = debug "genTAG_figure" $
                     do html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                        cap_  <- genTAG_figcaption cm $ iter n
                        lift $ frequency [ (1, return $ figure $ cap_ <> html_)
                                         , (1, return $ figure $ html_ <> cap_)
                                         , (1, return $ figure html_)] 


genTAG_figcaption :: Context -> Int -> GenState HasMain Html
genTAG_figcaption _ 0  = lift $ return $ toHtml "FIGCAPTION1"
genTAG_figcaption cm n = debug "genTAG_figcaption" $
                         do html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                            lift $ frequency [ (1, return $ toHtml "FIGCAPTION2")
                                             , (4, return $ figcaption html_)] 

genTAG_footer :: Context -> Int -> GenState HasMain Html
genTAG_footer _ 0  = lift $ return $ toHtml "FOOTER1"
genTAG_footer cm n = debug "genTAG_footer" $
                     do html_ <- arbPhrasingContent ( push (CExcl [TAG_HEADER, TAG_FOOTER, TAG_MAIN])
                                                    $ push CFlow cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "FOOTER2")
                                         , (4, return $ footer html_)] 
 
genTAG_form :: Context -> Int -> GenState HasMain Html
genTAG_form _  0 = lift $ return $ toHtml "FORM1"
genTAG_form cm n = debug "genTAG_form" $
                   do html_ <- arbPhrasingContent ( push (CExcl [TAG_FORM]) 
                                                  $ push CFlow cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "FORM2")
                                       , (4, return $ H.form html_)]
 

genTAG_h1 :: Context -> Int -> GenState HasMain Html
genTAG_h1 _  0 = lift $ return Empty
genTAG_h1 cm n = debug "genTAG_h1" $
                 do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ h1 html_)]
 
genTAG_h2 :: Context -> Int -> GenState HasMain Html
genTAG_h2 _  0 = lift $ return Empty
genTAG_h2 cm n = debug "genTAG_h2" $
                 do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ h2 html_)]

 
genTAG_h3 :: Context -> Int -> GenState HasMain Html
genTAG_h3 _  0 = lift $ return Empty
genTAG_h3 cm n = debug "genTAG_h3" $
                 do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ h3 html_)]

 
genTAG_h4 :: Context -> Int -> GenState HasMain Html
genTAG_h4 _  0 = lift $ return Empty
genTAG_h4 cm n = debug "genTAG_h4" $
                 do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ h4 html_)]

 
genTAG_h5 :: Context -> Int -> GenState HasMain Html
genTAG_h5 _  0 = lift $ return Empty
genTAG_h5 cm n = debug "genTAG_h5" $
                 do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ h5 html_)]

 
genTAG_h6 :: Context -> Int -> GenState HasMain Html
genTAG_h6 _  0 = lift $ return Empty
genTAG_h6 cm n = debug "genTAG_h6" $
                 do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ h6 html_)]


genTAG_header :: Context -> Int -> GenState HasMain Html
genTAG_header _  0 = lift $ return $ toHtml "HEADER1"
genTAG_header cm n = debug "genTAG_header" $
                     do html_ <- arbPhrasingContent (push (CExcl [TAG_HEADER, TAG_FOOTER, TAG_MAIN]) $ push CFlow cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "HEADER2")
                                         , (4, return $ header html_)]

 
genTAG_hgroup :: Context -> Int -> GenState HasMain Html
genTAG_hgroup _  0 = lift $ return Empty
genTAG_hgroup cm n = debug "genTAG_hgroup" $ do 
  hs_ <- liftM mconcat $ resizeState 3 
                     $ listOfState1 
                     $ oneofState 
                     $ mkGens cm (iter n) [ genTAG_h1
                                          , genTAG_h2
                                          , genTAG_h3
                                          , genTAG_h4
                                          , genTAG_h5
                                          , genTAG_h6]
  tmp_ <- liftM mconcat $ resizeState 3 $ listOfState $ genTAG_template cm $ iter n
  if BI.null hs_ 
  then genTAG_hgroup cm $ iter n
  else lift $ return $ hgroup $ hs_ <> tmp_


genTAG_hr :: Context -> Int -> GenState HasMain Html
genTAG_hr _ _ = debug "genTAG_hr" $ lift $ return hr
 
genTAG_i :: Context -> Int -> GenState HasMain Html
genTAG_i _  0 = lift $ return $ toHtml "I1"
genTAG_i cm n = debug "genTAG_i" $ 
                do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                   lift $ frequency [ (1, return $ toHtml "I2")
                                    , (4, return $ i html_)]

 
genTAG_iframe :: Context -> Int -> GenState HasMain Html
genTAG_iframe _  0 = lift $ return $ toHtml "IFRAME1"
genTAG_iframe cm n = debug "genTAG_iframe" $
                     do --html_ <- arbHtmlHTML cm $ iter n 
                        -- html_ <- arbPhrasingContent cm $ iter n
                        html_ <- return $ toHtml "IFRAME3"
                        lift $ frequency [ (1, return $ toHtml "IFRAME2")
                                         , (4, return $ iframe html_)]
 
genTAG_img :: Context -> Int -> GenState HasMain Html
genTAG_img _ _ = debug "genTAG_img" $
                 lift $ return $ img ! src (toValue "src") ! alt (toValue "alt")

 
genTAG_input :: Context -> Int -> GenState HasMain Html
genTAG_input _ _ = debug "genTAG_input" $
                   lift $ return input 

 
genTAG_ins :: Context -> Int -> GenState HasMain Html
genTAG_ins _  0 = lift $ return $ toHtml "INS1"
genTAG_ins cm n = debug "genTAG_ins" $
                  do html_ <- arbPhrasingContent cm $ iter n
                     lift $ frequency [ (1, return $ toHtml "INS2")
                                      , (4, return $ ins html_)]

 
genTAG_kbd :: Context -> Int -> GenState HasMain Html
genTAG_kbd _  0 = lift $ return $ toHtml "KBD1"
genTAG_kbd cm n = debug "genTAG_kbd" $
                  do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "KBD2")
                                      , (4, return $ kbd html_)]

 
genTAG_keygen :: Context -> Int -> GenState HasMain Html
genTAG_keygen _ _ = debug "genTAG_keygen" $
                    lift $ return keygen
 

genTAG_label :: Context -> Int -> GenState HasMain Html
genTAG_label _  0 = lift $ return $ toHtml "LABEL1"
genTAG_label cm n = debug "genTAG_label" $
                    do html_ <- arbPhrasingContent ( push (CExcl labelableElements) 
                                                   $ push (CExcl [TAG_LABEL])
                                                   $ push CPhrasing cm) $ iter n
                       lift $ frequency [ (1, return $ toHtml "LABEL2")
                                        , (4, return $ H.label html_)]


genTAG_legend :: Context -> Int -> GenState HasMain Html
genTAG_legend _  0 = lift $ return $ toHtml "LEGEND1"
genTAG_legend cm n = debug "genTAG_legend" $
                     do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "LEGEND2")
                                         , (4, return $ legend html_)]

genTAG_li :: Context -> Int -> GenState HasMain Html
genTAG_li _  0 = lift $ return Empty
genTAG_li cm n = debug "genTAG_li" $
                 do html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ li html_)]


genTAG_map :: Context -> Int -> GenState HasMain Html
genTAG_map _  0 = lift $ return $ toHtml "MAP1"
genTAG_map cm n = debug "genTAG_map" $
                  do html_ <- arbPhrasingContent (push (CIncl [TAG_MAP]) cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "MAP2")
                                      , (4, return $ (H.map html_) ! name (toValue "name"))]

 
genTAG_mark :: Context -> Int -> GenState HasMain Html
genTAG_mark _  0 = lift $ return $ toHtml "MARK1"
genTAG_mark cm n = debug "genTAG_mark" $
                   do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "MARK2")
                                       , (4, return $ mark html_)]

 
genTAG_math :: Context -> Int -> GenState HasMain Html
genTAG_math _ _ = debug "genTAG_math" $
                  lift $ return $ math Empty
 
genTAG_menu :: Context -> Int -> GenState HasMain Html
genTAG_menu _  0 = lift $ return $ toHtml "MENU1"
genTAG_menu cm n = debug "genTAG_menu" $
                   do flow_ <- arbPhrasingContent (push CPalpable cm) $ iter n
                      lis_  <- resizeState 3 $ listOfState $ 
                               genTAG_li (push CPalpable cm) (iter n) 
                      scrp_ <- resizeState 3 $ listOfState $ oneofState 
                               $ mkGens (push CPalpable cm) (iter n)
                                     [ genTAG_script
                                     , genTAG_template]
                      toolbar_ <- lift $ frequency [ (1, return flow_)
                                                   , (1, return $ mconcat lis_)
                                                   , (1, return $ mconcat scrp_)]
                      popup_ <- resizeState 3 $ listOfState $ oneofState 
                                $ mkGens (push CFlow cm) (iter n)
                                      [ genTAG_menuitem
                                      , genTAG_hr
                                      , genTAG_menu_popup
                                      , genTAG_script
                                      , genTAG_template]
                      lift $ frequency [ (1, return $ toHtml "MENU2")
                                       -- , (4, return $ (menu toolbar_) ! type_ (toValue "toolbar"))
                                       , (4, return $ (menu $ mconcat popup_) ! type_ (toValue "popup"))]

 
genTAG_menuitem :: Context -> Int -> GenState HasMain Html
genTAG_menuitem _ _ = debug "genTAG_menuitem" $
                      lift $ return $ menuitemT Empty

genTAG_menu_popup :: Context -> Int -> GenState HasMain Html
genTAG_menu_popup cm n = debug "genTAG_menu_popup" $
                         do html_ <- genTAG_menu_popup cm n
                            lift $ return (html_ ! type_ (toValue "popup"))        
 
genTAG_meter :: Context -> Int -> GenState HasMain Html
genTAG_meter _  0 = lift $ return $ toHtml "METER1"
genTAG_meter cm n = debug "genTAG_meter" $
                    do html_ <- arbPhrasingContent ( push (CExcl [TAG_METER])
                                                   $ push CPhrasing cm) $ iter n
                       lift $ frequency [ (1, return $ toHtml "METER2")
                                        , (4, return $ meter html_ ! value (toValue "1"))]
 
 
genTAG_object :: Context -> Int -> GenState HasMain Html
genTAG_object _  0 = lift $ return $ toHtml "OBJECT1"
genTAG_object cm n = debug "genTAG_object" $
                     do params_ <- resizeState 3 $ listOfState $ genTAG_param cm $ iter n
                        html_   <- arbPhrasingContent cm $ iter n
                        lift $ frequency [ (1, return $ toHtml "OBJECT2")
                                         , (4, return $ object (mconcat params_ <> html_) ! data_ (toValue "."))]

 
genTAG_ol :: Context -> Int -> GenState HasMain Html
genTAG_ol _  0 = lift $ return $ toHtml "OL1"
genTAG_ol cm n = debug "genTAG_ol" $
                 do html_ <- resizeState 3 $ listOfState $ oneofState 
                             $ mkGens (push CFlow cm) (iter n)
                                   [ genTAG_li
                                   , genTAG_script
                                   , genTAG_template]
                    lift $ frequency [ (1, return $ toHtml "OL2")
                                     , (4, return $ ol $ mconcat html_)]


genTAG_optgroup :: Context -> Int -> GenState HasMain Html
genTAG_optgroup _  0 = lift $ return Empty
genTAG_optgroup cm n = debug "genTAG_optgroup" $
                       liftM (flip (!) (A.label $ toValue "a") . optgroup . mconcat) 
                       $ resizeState 3 $ listOfState 
                       $ oneofState $ mkGens cm (iter n) [ genTAG_option
                                                         , genTAG_script
                                                         , genTAG_template]

genTAG_option :: Context -> Int -> GenState HasMain Html
genTAG_option _ _ = debug "genTAG_option" $
                    lift $ frequency [ (1, return $ (option Empty) ! A.label (toValue "a") ! value (toValue "1"))
                                     , (1, return $ (option $ toHtml "OPTION1") ! A.label (toValue "a"))
                                     , (1, return $ (option $ toHtml "OPTION2"))
                                     ]
 

genTAG_output :: Context -> Int -> GenState HasMain Html
genTAG_output _  0 = lift $ return $ toHtml "OUTPUT1"
genTAG_output cm n = debug "genTAG_output" $
                     do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "OUTPUT2")
                                         , (4, return $ output html_)]
 

genTAG_p :: Context -> Int -> GenState HasMain Html
genTAG_p _  0 = lift $ return $ toHtml "P1"
genTAG_p cm n = debug "genTAG_p" $
                do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                   lift $ frequency [ (1, return $ toHtml "P2")
                                    , (4, return $ p html_)]


genTAG_param :: Context -> Int -> GenState HasMain Html
genTAG_param _ _ = debug "genTAG_param" $
                   lift $ return $ param ! name (toValue "name") ! value (toValue "value")

 
genTAG_pre :: Context -> Int -> GenState HasMain Html
genTAG_pre _  0 = lift $ return $ toHtml "PRE1"
genTAG_pre cm n = debug "genTAG_pre" $
                  do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "PRE2")
                                      , (4, return $ pre html_)]


genTAG_progress :: Context -> Int -> GenState HasMain Html
genTAG_progress _  0 = lift $ return $ toHtml "PROGRESS1"
genTAG_progress cm n = debug "genTAG_progress" $
                       do html_ <- arbPhrasingContent ( push (CExcl [TAG_PROGRESS]) 
                                                      $ push CPhrasing cm) $ iter n
                          lift $ frequency [ (1, return $ toHtml "PROGRESS2")
                                           , (4, return $ progress html_)]


genTAG_q :: Context -> Int -> GenState HasMain Html
genTAG_q _  0 = lift $ return $ toHtml "Q1"
genTAG_q cm n = debug "genTAG_q" $
                do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                   lift $ frequency [ (1, return $ toHtml "Q2")
                                    , (4, return $ q html_)]

 
genTAG_rp :: Context -> Int -> GenState HasMain Html
genTAG_rp _ _ = debug "genTAG_rp" $
                lift $ return $ rp $ toHtml "RP"


genTAG_rt :: Context -> Int -> GenState HasMain Html
genTAG_rt _ _ = debug "genTAG_rt" $
                lift $ return $ rt $ toHtml "RT"


genTAG_ruby :: Context -> Int -> GenState HasMain Html
genTAG_ruby _ 0  = lift $ return $ toHtml "RUBY1"
genTAG_ruby cm n = debug "genTAG_ruby" $
                   do html1_ <- liftM mconcat $ resizeState 3 $ listOfState1 $ genTAG_rt cm $ iter n
                      html2_ <- liftM mconcat $ resizeState 3 $ listOfState1 $ liftM3 (\p1 h p2 -> p1 <> h <> p2) (genTAG_rp cm $ iter n) (genTAG_rt cm $ iter n) (genTAG_rp cm $ iter n)
                      lift $ frequency [ (1, return $ toHtml "RUBY2")
                                       , (4, return $ ruby html1_)
                                       , (4, return $ ruby html2_)]
 

genTAG_s :: Context -> Int -> GenState HasMain Html
genTAG_s _ 0 = lift $ return $ toHtml "S1"
genTAG_s cm n = debug "genTAG_s" $
                do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                   lift $ frequency [ (1, return $ toHtml "S2")
                                    , (4, return $ s html_)]
 

genTAG_samp :: Context -> Int -> GenState HasMain Html
genTAG_samp _ 0 = lift $ return $ toHtml "SAMP1"
genTAG_samp cm n = debug "genTAG_samp" $
                   do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "SAMP2")
                                       , (4, return $ samp html_)]
 
 
genTAG_select :: Context -> Int -> GenState HasMain Html
genTAG_select _  0 = lift $ return Empty
genTAG_select cm n = debug "genTAG_select" $
                     liftM (select . mconcat) 
                       $ resizeState 3 $ listOfState 
                       $ oneofState $ mkGens cm (iter n) [ genTAG_option
                                                         , genTAG_optgroup
                                                         , genTAG_template]


genTAG_small :: Context -> Int -> GenState HasMain Html
genTAG_small _ 0 = lift $ return $ toHtml "SMALL1"
genTAG_small cm n = debug "genTAG_small" $
                    do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                       lift $ frequency [ (1, return $ toHtml "SMALL2")
                                        , (4, return $ small html_)]


genTAG_source :: Context -> Int -> GenState HasMain Html
genTAG_source _ _ = debug "genTAG_source" $
                    lift $ return $ source ! src (toValue ".")
 
genTAG_span :: Context -> Int -> GenState HasMain Html
genTAG_span _ 0 = lift $ return $ toHtml "SPAN1"
genTAG_span cm n = debug "genTAG_span" $
                   do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                      lift $ frequency [ (1, return $ toHtml "SPAN2")
                                       , (4, return $ H.span html_)]
 

genTAG_strong :: Context -> Int -> GenState HasMain Html
genTAG_strong _ 0 = lift $ return $ toHtml "STRONG1"
genTAG_strong cm n = debug "genTAG_strong" $
                     do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                        lift $ frequency [ (1, return $ toHtml "STRONG2")
                                         , (4, return $ strong html_)]
 

genTAG_sub :: Context -> Int -> GenState HasMain Html
genTAG_sub _  0 = lift $ return $ toHtml "SUB1"
genTAG_sub cm n = debug "genTAG_sub" $
                  do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "SUB2")
                                      , (4, return $ sub html_)]


genTAG_summary :: Context -> Int -> GenState HasMain Html
genTAG_summary _  0 = lift $ return $ H.summary Empty
genTAG_summary cm n = debug "genTAG_summary" $
                      do phr_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                         hea_ <- oneofState $ mkGens cm (iter n) $ context2gens $ push CHeading cm
                         lift $ frequency [ (1, return $ H.summary phr_)
                                          , (1, return $ H.summary hea_)]

 
genTAG_sup :: Context -> Int -> GenState HasMain Html
genTAG_sup _  0 = lift $ return $ toHtml "SUP1"
genTAG_sup cm n = debug "genTAG_sup" $
                  do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "SUP2")
                                      , (4, return $ sup html_)]
 
genTAG_svg :: Context -> Int -> GenState HasMain Html
genTAG_svg _ _ = debug "genTAG_svg" $
                 lift $ return $ svg Empty
 
genTAG_table :: Context -> Int -> GenState HasMain Html
genTAG_table _  0 = lift $ return $ toHtml "TABLE1"
genTAG_table cm n = debug "genTAG_table" $
                    do cap_    <- optional $ genTAG_caption cm $ iter n
                       colgr_  <- resizeState 3 $ listOfState $ genTAG_colgroup cm $ iter n
                       thead_  <- optional $ genTAG_thead cm $ iter n
                       tfoot1_ <- optional $ genTAG_tfoot cm $ iter n
                       tbody_  <- resizeState 3 $ listOfState $ genTAG_tbody cm $ iter n
                       tr_     <- resizeState 3 $ listOfState1 $ genTAG_tr cm $ iter n 
                       tfoot2_ <- optional $ genTAG_tfoot cm $ iter n
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


genTAG_caption :: Context -> Int -> GenState HasMain Html
genTAG_caption _  0 = lift $ return Empty
genTAG_caption cm n = debug "genTAG_caption" $
                      do html_ <- arbPhrasingContent ( push (CExcl [TAG_TABLE]) 
                                                     $ push CFlow cm) $ iter n
                         lift $ frequency [ (1, return Empty)
                                          , (4, return $ caption html_)]

genTAG_colgroup :: Context -> Int -> GenState HasMain Html
genTAG_colgroup _ _ = debug "genTAG_colgroup" $
                      lift $ return Empty
-- genTAG_colgroup _  0 = lift $ return Empty
-- genTAG_colgroup cm n = do html_ <- resizeState 3 $ listOfState 
--                                    $ oneofState $ mkGens cm (iter n)
--                                          [ genTAG_col
--                                          , genTAG_template]
--                           lift $ frequency [ (1, return Empty)
--                                            , (4, return $ (colgroup Empty) ! A.span (toValue "1"))
--                                            , (4, return $ (colgroup $ mconcat html_) )]



genTAG_col :: Context -> Int -> GenState HasMain Html
genTAG_col _ _ = debug "genTAG_col" $ lift $ return col


genTAG_thead :: Context -> Int -> GenState HasMain Html
genTAG_thead _  0 = lift $ return Empty
genTAG_thead cm n = debug "genTAG_thead" $
                    do html_ <- resizeState 3 $ listOfState 
                                $ oneofState $ mkGens cm (iter n)
                                      [ genTAG_tr
                                      , genTAG_template
                                      , genTAG_script]
                       lift $ frequency [ (1, return Empty)
                                        , (4, return $ thead $ mconcat html_)]


genTAG_tfoot :: Context -> Int -> GenState HasMain Html
genTAG_tfoot _  0 = lift $ return Empty
genTAG_tfoot cm n = debug "genTAG_tfoot" $
                    do html_ <- resizeState 3 $ listOfState 
                                $ oneofState $ mkGens cm (iter n)
                                      [ genTAG_tr
                                      , genTAG_template
                                      , genTAG_script]
                       lift $ frequency [ (1, return Empty)
                                        , (4, return $ tfoot $ mconcat html_)]


genTAG_tbody :: Context -> Int -> GenState HasMain Html
genTAG_tbody _  0 = lift $ return Empty
genTAG_tbody cm n = debug "genTAG_tbody" $
                    do html_ <- resizeState 3 $ listOfState 
                                $ oneofState $ mkGens cm (iter n)
                                      [ genTAG_tr
                                      , genTAG_template
                                      , genTAG_script]
                       lift $ frequency [ (1, return Empty)
                                        , (4, return $ tbody $ mconcat html_)]


genTAG_tr :: Context -> Int -> GenState HasMain Html
genTAG_tr _  0 = lift $ return Empty
genTAG_tr cm n = debug "genTAG_tr" $
                 do html_ <- resizeState 3 $ listOfState1 
                             $ oneofState $ mkGens cm (iter n)
                                   [ genTAG_td
                                   , genTAG_th
                                  --  , genTAG_template
                                   -- , genTAG_script
                                   ]
                    lift $ frequency [ (1, return Empty)
                                     , (4, return $ tr $ mconcat html_)]


genTAG_td :: Context -> Int -> GenState HasMain Html
genTAG_td _  0 = lift $ return $ td $ toHtml "TD1"
genTAG_td cm n = debug "genTAG_td" $
                 do html_ <- arbPhrasingContent (push CFlow cm) $ iter n
                    lift $ frequency [ (1, return $ td $ toHtml "TD1")
                                     , (4, return $ td html_)]

 
genTAG_th :: Context -> Int -> GenState HasMain Html
genTAG_th _  0 = lift $ return $ th $ toHtml "TH1"
genTAG_th cm n = debug "genTAG_th" $
                 do html_ <- arbPhrasingContent ( push (CExcl $ [TAG_HEADER, TAG_FOOTER]
                                                        ++ sectioningContent 
                                                        ++ headingContent)
                                                $ push CFlow cm) $ iter n
                    lift $ frequency [ (1, return $ th $ toHtml "TH1")
                                     , (4, return $ th html_)]


genTAG_template :: Context -> Int -> GenState HasMain Html
genTAG_template _ _ = debug "genTAG_template" $
                      lift $ return $ template $ toHtml "TEMPLATE"
 
genTAG_textarea :: Context -> Int -> GenState HasMain Html
genTAG_textarea _ _ = debug "genTAG_textarea" $
                      lift $ return $ textarea $ toHtml "TEXTAREA" 

 
genTAG_time :: Context -> Int -> GenState HasMain Html
genTAG_time _  _ = debug "genTAG_time" $
                   lift $ return $ time $ toHtml "10:00"
-- genTAG_time _  0 = lift $ return $ toHtml "TIME1"
-- genTAG_time cm n = do html_ <- arbPhrasingContent (push (Right CPhrasing) cm) $ iter n
--                       lift $ frequency [ (1, return $ toHtml "TIME2")
--                                        , (4, return $ time html_)]
 

genTAG_track :: Context -> Int -> GenState HasMain Html
genTAG_track _ _ = debug "genTAG_track" $
                   lift $ return $ track ! src (toValue ".")

genTAG_u :: Context -> Int -> GenState HasMain Html
genTAG_u _  0 = lift $ return $ toHtml "U1"
genTAG_u cm n = debug "genTAG_u" $
                do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                   lift $ frequency [ (1, return $ toHtml "U2")
                                    , (4, return $ u html_)]

 
genTAG_ul :: Context -> Int -> GenState HasMain Html
genTAG_ul _  0 = lift $ return $ toHtml "UL1"
genTAG_ul cm n = debug "genTAG_ul" $
                 do html_ <- resizeState 3 $ listOfState $ oneofState 
                             $ mkGens (push CFlow cm) (iter n)
                                   [ genTAG_li
                                   , genTAG_script
                                   , genTAG_template]
                    lift $ frequency [ (1, return $ toHtml "UL2")
                                     , (4, return $ ol $ mconcat html_)]

 
genTAG_var :: Context -> Int -> GenState HasMain Html
genTAG_var _  0 = lift $ return $ toHtml "VAR1"
genTAG_var cm n = debug "genTAG_var" $
                  do html_ <- arbPhrasingContent (push CPhrasing cm) $ iter n
                     lift $ frequency [ (1, return $ toHtml "VAR2")
                                      , (4, return $ var html_)]


genTAG_video :: Context -> Int -> GenState HasMain Html
genTAG_video _ 0 = lift $ return $ toHtml "VIDEO"
genTAG_video cm n = debug "genTAG_video" $
                    do html_    <- arbPhrasingContent (push (CExcl mediaElements) cm) $ iter n
                       tracks_  <- resizeState 3 $ listOfState $ genTAG_track cm $ iter n
                       sources_ <- resizeState 3 $ listOfState $ genTAG_source cm $ iter n 
                       lift $ frequency [ (1, return $ video $ (mconcat (sources_ ++ tracks_)) <> html_)
                                        , (1, return $ (video $ ((mconcat tracks_) <> html_)) ! src (toValue "."))]


genTAG_wbr :: Context -> Int -> GenState HasMain Html
genTAG_wbr _ _ = debug "genTAG_wbr" $
                 lift $ return wbr 
