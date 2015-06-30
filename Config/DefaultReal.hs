module Config.DefaultReal where

import Html5C.Tags
import Config.Data


-- | http://webmasters.stackexchange.com/questions/11406/recent-statistics-on-html-usage-in-the-wild
defTagFreqTblReal :: [(Int, HTML_TAG)]
defTagFreqTblReal = 
    [(39923975, TAG_A), 
     (20356, TAG_ABBR), 
     (9725, TAG_ADDRESS), 
     (221226, TAG_AREA),
     (0, TAG_ARTICLE),
     (0, TAG_ASIDE),
     (0, TAG_AUDIO),
     (4419975, TAG_B),
     (42147, TAG_BASE),
     (5, TAG_BDI),
     (425, TAG_BDO),
     (42869, TAG_BLOCKQUOTE),
     (17266040, TAG_BR),
     (23417, TAG_BUTTON),
     (0, TAG_CANVAS),
     (8633, TAG_CAPTION),
     (47406, TAG_CITE),
     (21190, TAG_CODE),
     (8191, TAG_COLGROUP),
     (40191, TAG_COL),
     (0, TAG_DATA),
     (0, TAG_DATE),
     (0, TAG_DATALIST),
     (4639, TAG_DEL),
     (0, TAG_DETAILS),
     (12974, TAG_DFN),
     (0, TAG_DIALOG),
     (15644761, TAG_DIV),
     (142403, TAG_DL),
     (521143, TAG_DD),
     (363085, TAG_DT),
     (363613, TAG_EM),
     (51864, TAG_EMBED),
     (55447, TAG_FIELDSET),
     (0, TAG_FIGURE),
     (0, TAG_FIGCAPTION),
     (0, TAG_FOOTER),
     (703914, TAG_FORM),
     (318542, TAG_H1),
     (477122, TAG_H2),
     (400073, TAG_H3),
     (162877, TAG_H4),
     (64141, TAG_H5),
     (24766, TAG_H6),
     (0, TAG_HEADER),
     (0, TAG_HGROUP),
     (712094, TAG_HR),
     (503215, TAG_I),
     (126827, TAG_IFRAME),
     (13539744, TAG_IMG),
     (3413040, TAG_INPUT),
     (5433, TAG_INS),
     (1482, TAG_KBD),
     (0, TAG_KEYGEN),
     (342376, TAG_LABEL),
     (32738, TAG_LEGEND),
     (1262896, TAG_LINK),
     (8531300, TAG_LI),
     (0, TAG_MAIN),
     (41401, TAG_MAP),
     (23, TAG_MARK),
     (0, TAG_MATH),
     (0, TAG_MENU),
     (1738, TAG_MENUITEM),
     (1809099, TAG_META),
     (0, TAG_METER),
     (0, TAG_NAV),
     (227474, TAG_NOSCRIPT),
     (53226, TAG_OBJECT),
     (28879, TAG_OL),
     (9319944, TAG_OPTION),
     (43329, TAG_OPTGROUP),
     (0, TAG_OUTPUT),
     (3762731, TAG_P),
     (204798, TAG_PARAM),
     (26118, TAG_PRE),
     (0, TAG_PROGRESS),
     (1638, TAG_Q),
     (0, TAG_RP),
     (0, TAG_RT),
     (0, TAG_RUBY),
     (24510, TAG_S),
     (1767, TAG_SAMP),
     (3368258, TAG_SCRIPT),
     (0, TAG_SECTION),
     (311449, TAG_SELECT),
     (242097, TAG_SMALL),
     (0, TAG_SOURCE),
     (7573226, TAG_SPAN),
     (1855448, TAG_STRONG),
     (292716, TAG_STYLE),
     (5634, TAG_SUB),
     (0, TAG_SUMMARY),
     (72331, TAG_SUP),
     (0, TAG_SVG),
     (5999791, TAG_TABLE),
     (0, TAG_TEMPLATE),
     (54044, TAG_TEXTAREA),
     (27083, TAG_THEAD),
     (61, TAG_TIME),
     (446862, TAG_TITLE),
     (15340012, TAG_TR),
     (29707781, TAG_TD),
     (393534, TAG_TH),
     (0, TAG_TRACK),
     (3282, TAG_TFOOT),
     (275069, TAG_TBODY),
     (227338, TAG_U),
     (1246231, TAG_UL),
     (3729, TAG_VAR),
     (0, TAG_VIDEO),
     (36995, TAG_WBR)]


normFreqTblReal :: [(Int, HTML_TAG)] -> [(Int, HTML_TAG)]
normFreqTblReal tbl = map (\(i, tag) -> (round ( toRational i / toRational mFreq), tag)) tbl
    where
      minFreq :: [(Int, HTML_TAG)] -> Int
      minFreq [] = error "tag's frequency table should not be empty"
      minFreq ts = foldl1 min_ $ map (\(i, _) -> i) ts

      min_ :: Int -> Int -> Int
      min_ i 0 = i
      min_ 0 j = j
      min_ i j = min i j

      mFreq = minFreq tbl


-- a                                       39923975
-- td                                      29707781
-- br                                      17266040
-- div                                     15644761
-- tr                                      15340012
-- img                                     13539744
-- option                                  9319944
-- li                                      8531300
-- span                                    7573226
-- table                                   5999791
-- font                                    5329688*
-- b                                       4419975
-- p                                       3762731
-- input                                   3413040
-- script                                  3368258
-- strong                                  1855448
-- meta                                    1809099
-- link                                    1262896
-- ul                                      1246231
-- hr                                      712094
-- form                                    703914
-- dd                                      521143
-- i                                       503215
-- center                                  483503*
-- h2                                      477122
-- title                                   446862
-- body                                    445094*
-- head                                    436058*
-- html                                    435397
-- h3                                      400073
-- th                                      393534
-- em                                      363613
-- dt                                      363085
-- label                                   342376
-- h1                                      318542
-- select                                  311449
-- style                                   292716
-- tbody                                   275069
-- nobr                                    251825*
-- small                                   242097
-- noscript                                227474
-- u                                       227338
-- area                                    221226
-- param                                   204798
-- h4                                      162877
-- dl                                      142403
-- iframe                                  126827
-- o                                       86064*
-- sup                                     72331
-- h5                                      64141
-- fieldset                                55447
-- textarea                                54044
-- object                                  53226
-- embed                                   51864
-- cite                                    47406
-- scr                                     47050*
-- tt                                      45748*
-- big                                     44210*
-- optgroup                                43329
-- blockquote                              42869
-- base                                    42147
-- map                                     41401
-- col                                     40191
-- wbr                                     36995
-- legend                                  32738
-- d                                       29939*
-- ol                                      28879
-- thead                                   27083
-- spacer                                  26848*
-- pre                                     26118
-- h6                                      24766
-- s                                       24510

-- button                                  23417
-- code                                    21190
-- rdf                                     20960*
-- abbr                                    20356
-- acronym                                 20186*
-- w                                       16818*
-- noindex                                 14038*
-- dfn                                     12974
-- marquee                                 11350*
-- v                                       11165*
-- strike                                  10522*
-- address                                 9725
-- description                             9058*
-- sc                                      8677*
-- caption                                 8633
-- st1                                     8374*
-- colgroup                                8191
-- item                                    8090*
-- pubdate                                 7160*
-- layer                                   6156*
-- sub                                     5634
-- ins                                     5433
-- category                                5106*
-- guid                                    4936*
-- document                                4678*
-- del                                     4639
-- frame                                   4548*
-- dc                                      4323*
-- image                                   4306*
-- var                                     3729
-- variable                                3292*
-- tfoot                                   3282
-- x                                       3120*
-- xs                                      2963*
-- zeroboard                               2870*
-- js                                      2825*
-- ilayer                                  2823*
-- frameset                                2738*
-- media                                   2669*
-- rx                                      2658*
-- author                                  2581*
-- c                                       2563*
-- h                                       2227*
-- ifr                                     2213*
-- xml                                     2134*
-- m                                       2007*
-- csobj                                   1972*
-- n                                       1948*
-- set                                     1872*
-- l                                       1870*
-- permits                                 1848*
-- samp                                    1767
-- menuitem                                1738
-- requires                                1732*
-- noframes                                1714*
-- z                                       1686*
-- this                                    1655*
-- q                                       1638
-- t                                       1629*
-- f                                       1622*
-- content                                 1612*
-- license                                 1533*
-- left                                    1501*
-- srch                                    1488*
-- kbd                                     1482
-- sfels                                   1466*
-- hs                                      1438*
-- para                                    1434*
-- comments                                1394*
-- changeimages                            1393*
-- list                                    1362*
-- actinic                                 1358*
-- csaction                                1352*
-- skype                                   1338*
-- myarr                                   1337*
-- index                                   1283*
-- blip                                    1279*
-- scri                                    1262*
-- mlp                                     1205*
-- e                                       1199*
-- url                                     1199*
-- basefont                                1167*
-- channel                                 1153*
-- if                                      1152*
-- u1                                      1151*
-- g                                       1137*
-- xsl                                     1137*
-- literal                                 1108*
-- rss                                     1105*
-- itemtemplate                            1053*
-- j                                       1036*
-- blink                                   1022*
-- len                                     1018*
-- id                                      1001*
-- align                                   1000*
