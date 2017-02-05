//Do the PING! animation
function goPing(pingH, pingV, iteration) {
    var PING = document.getElementById(PING);
    if (iteration == 1) {
        PING.src = "img/ping1.png";
        moveObjTo(PING, pingH, pingV);
        window.setTimeout("goPing(" + pingH + "," + pingV + ",2);", 80);
    } else if (iteration == 2) {
        PING.src = "img/ping2.png";
        window.setTimeout("goPing(0,0,3);", 80);
    } else {
        moveObjTo(PING, -100, -100);
    }
}

//Simple move function
function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

/*
 * Types:
 * pingH: int
 * pingV: int
 * iteration: int
 */