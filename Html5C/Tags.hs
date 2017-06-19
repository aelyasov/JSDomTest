module Html5C.Tags where 

import Data.Char
import Debug.Trace


data HTML_TAG = TAG_A
              | TAG_ABBR
              | TAG_ADDRESS
              | TAG_AREA
              | TAG_ARTICLE
              | TAG_ASIDE
              | TAG_AUDIO
              | TAG_B
              | TAG_BASE
              | TAG_BDI
              | TAG_BDO
              | TAG_BLOCKQUOTE
              | TAG_BODY
              | TAG_BR
              | TAG_BUTTON
              | TAG_CANVAS
              | TAG_CAPTION
              | TAG_CITE
              | TAG_CODE
              | TAG_COLGROUP
              | TAG_COL
              | TAG_DATA
              | TAG_DATE
              | TAG_DATALIST
              | TAG_DEL
              | TAG_DETAILS
              | TAG_DFN
              | TAG_DIALOG
              | TAG_DIV
              | TAG_DL
              | TAG_DD
              | TAG_DT
              | TAG_EM
              | TAG_EMBED
              | TAG_FIELDSET
              | TAG_FIGURE
              | TAG_FIGCAPTION
              | TAG_FOOTER
              | TAG_FORM
              | TAG_H1
              | TAG_H2
              | TAG_H3
              | TAG_H4
              | TAG_H5
              | TAG_H6
              | TAG_HEADER
              | TAG_HGROUP
              | TAG_HR
              | TAG_I
              | TAG_IFRAME
              | TAG_IMG
              | TAG_INPUT
              | TAG_INS
              | TAG_KBD
              | TAG_KEYGEN
              | TAG_LABEL
              | TAG_LEGEND
              | TAG_LINK
              | TAG_LI
              | TAG_MAIN
              | TAG_MAP
              | TAG_MARK
              | TAG_MATH
              | TAG_MENU
              | TAG_MENUITEM
              | TAG_META
              | TAG_METER
              | TAG_NAV
              | TAG_NOSCRIPT
              | TAG_OBJECT
              | TAG_OL
              | TAG_OPTION
              | TAG_OPTGROUP
              | TAG_OUTPUT
              | TAG_P
              | TAG_PARAM
              | TAG_PRE
              | TAG_PROGRESS
              | TAG_Q
              | TAG_RP
              | TAG_RT
              | TAG_RUBY
              | TAG_S
              | TAG_SAMP
              | TAG_SCRIPT
              | TAG_SECTION
              | TAG_SELECT
              | TAG_SMALL
              | TAG_SOURCE
              | TAG_SPAN
              | TAG_STRONG
              | TAG_STYLE
              | TAG_SUB
              | TAG_SUMMARY
              | TAG_SUP
              | TAG_SVG
              | TAG_TABLE
              | TAG_TEMPLATE
              | TAG_TEXTAREA
              | TAG_THEAD
              | TAG_TIME
              | TAG_TITLE
              | TAG_TR
              | TAG_TD
              | TAG_TH
              | TAG_TRACK
              | TAG_TFOOT
              | TAG_TBODY
              | TAG_U
              | TAG_UL
              | TAG_VAR
              | TAG_VIDEO
              | TAG_WBR
              deriving (Show, Read, Eq)


str2HtmlTag :: String -> HTML_TAG
str2HtmlTag tag = read ("TAG_" ++ (map toUpper tag)) :: HTML_TAG
