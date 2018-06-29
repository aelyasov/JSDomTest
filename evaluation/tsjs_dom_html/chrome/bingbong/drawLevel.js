//Draws the level onto the screen, and then clears it again
function drawLevel(iteration, theLevel, myCenterH, myCenterV) {
    var LEVEL = document.getElementById("LEVEL");
    var LEVEL1 = document.getElementById("LEVEL1");
    var LEVEL2 = document.getElementById("LEVEL2");

    if (iteration == 1) {
        //Set text details
        //Int to Str
        var stringLevel = theLevel + '';
        //Pad string
        var i = 0;
        for (i = 1; i <= 2; i++) {
            if (stringLevel.length < 2) {
                stringLevel = '0' + stringLevel;
            }
        }
        for (i = 1; i <= 2; i++) {
            document.getElementById('LEVEL' + i).src = "img/num" + stringLevel.charAt(i - 1) + ".png";
        }
        //Move on-screen
        moveObjTo(LEVEL, myCenterH - 75, myCenterV + 30);
        moveObjTo(LEVEL1, myCenterH + 25, myCenterV + 25);
        moveObjTo(LEVEL2, myCenterH + 50, myCenterV + 25);
        window.setTimeout("drawLevel(2);", 1400);
    } else {
        moveObjTo(LEVEL, -100, -100);
        moveObjTo(LEVEL1, -100, -100);
        moveObjTo(LEVEL2, -100, -100);
    }
}

//Simple move function
function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

/*
 * Types:
 * iteration: int
 * theLevel: int
 * myCenterH: int
 * myCenterV: int
 */