/*t dom:int:int */
function test(eventY, myHeight) {
    var PADDLEPOWERBAR = document.getElementById("PADDLEPOWERBAR");
    var PADDLEPOWEROUTLINE = document.getElementById("PADDLEPOWEROUTLINE");
    var paddlePower = eventY - 100;
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

function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}