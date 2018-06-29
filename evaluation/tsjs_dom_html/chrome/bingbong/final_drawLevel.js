/*t dom:int:int:int:int */
function test(iteration, theLevel, myCenterH, myCenterV) {
    var LEVEL = document.getElementById("LEVEL");
    var LEVEL1 = document.getElementById("LEVEL1");
    var LEVEL2 = document.getElementById("LEVEL2");
    if (iteration == 1) {
        var stringLevel = theLevel + '';
        for (var i = 1; i <= 2; i++) {
            if (stringLevel.length < 2) {
                stringLevel = '0' + stringLevel;
            }
        }
        for (var i = 1; i <= 2; i++) {
            document.getElementById('LEVEL' + i).src = "img/num" + stringLevel.charAt(i - 1) + ".png";
        }
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

function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}