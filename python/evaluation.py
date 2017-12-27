#!/usr/bin/python

from subprocess import call
from os.path import basename, splitext, join
from time import time
from datetime import datetime
import sys
from os import makedirs, chdir

confix = "./evaluation/confix/"
tsjs_dom_html = "./evaluation/tsjs_dom_html/"

sudoku = confix + "sudoku/final_"
phormer = confix + "Phormer/final_"
hotel = confix + "HotelReservationSystem/final_"
apophis = tsjs_dom_html + "chrome/apophis/final_"
bingbong = tsjs_dom_html + "chrome/bingbong/final_"
burncanvas = tsjs_dom_html + "chrome/burncanvas/final_"
csjs = "./evaluation/computer-science-in-javascript/final_"
mathjs = "./evaluation/mathjs/final_"

out_folder = "./evaluation/result"

case_studies = [
    # # sudoku + "getPossibleNumbers.js",
    # sudoku + "helpMe.js",          # +
    # sudoku + "isGameFinished.js",  # +
    # sudoku + "newGame.js",         # +
    # sudoku + "revealAll.js",       # +
    # # sudoku + "showCell.js",
    # sudoku + "shuffleBoard.js",    # -
    # # sudoku + "switchLevel.js",

    # phormer + "toggleInfo.js",     # -
    # # phormer + "update.js",
    # # phormer + "updateIndic.js",

    # # hotel + "RequiredField.js",
    # hotel + "isValidCard.js",        # +
    # hotel + "isValidMasterCard.js",  # +
    # hotel + "isValidVISA.js",        # +
    # hotel + "validateNumber.js",

    # apophis + "doRain.js",
    # apophis + "drawShields.js",
    # apophis + "fireMeteor.js",
    # apophis + "getReady.js",
    # apophis + "initShields.js",     # +-

    # bingbong + "brickJiggler.js",   # +
    # bingbong + "doPaddlePower.js",  # -
    # bingbong + "drawLevel.js",      # -
    # bingbong + "goPing.js",

    # bingbong + "initBricks.js",     # -

    burncanvas + "do_draw.js",      # -
    # burncanvas + "modify_region.js",

    # csjs + "luhn-algorithm.js",
    # csjs + "quicksort_partition.js",

    # # mathjs + "probability_gamma.js"  # +
]

eval_type = sys.argv[1]
st = datetime.fromtimestamp(time()).strftime('%Y-%m-%d_%H:%M:%S')
cs_outfolder = join(out_folder, eval_type, st)
makedirs(cs_outfolder)
for cs in case_studies:
    cs_base = basename(cs)
    cs_outfile = splitext(cs_base)[0]
    cs_out = cs_outfile[6:] + "_" + eval_type + ".out"
    cs_output = cs_outfolder + "/" + cs_out
    print st, "Processing case study: ", cs
    print st, "Writing output to: ", cs_output
    call(["stack", "exec", "JSDomTest", cs, cs_output])
    # call(["cabal", "run", cs, cs_output])
