//Draw (and get) the paddle strength
//Scales from 0 - 100 based on distance -- from 100 pixels from top to 100 pixels from bottom
function doPaddlePower(eventY, myHeight, paddlePower) {
    var PADDLEPOWERBAR = document.getElementById("PADDLEPOWERBAR");
    var PADDLEPOWEROUTLINE = document.getElementById("PADDLEPOWEROUTLINE");
    paddlePower = eventY - 100;
    if (paddlePower < 1) {
        paddlePower = 0;
    }
    paddlePower = (paddlePower / (myHeight - 200)) * 100;
    if (paddlePower > 100) {
        paddlePower = 100;
    }
    paddlePower = 100 - paddlePower;
    PADDLEPOWERBAR.height = (153 * paddlePower / 100);
    moveObjTo(PADDLEPOWEROUTLINE, 20, myHeight - 200);
    moveObjTo(PADDLEPOWERBAR, 26, myHeight - 25 - PADDLEPOWERBAR.height);
}


//Simple move function
function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

/*
 * Types:
 * eventY: int
 * myHeight: int
 * paddlePower: int
 */