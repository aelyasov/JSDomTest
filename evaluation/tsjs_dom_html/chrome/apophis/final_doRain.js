/*t dom:string:int:int:int:int:int:int */
function test(id, locX, locY, speed, tankLive, myHeight, myWidth) {
    var obj = document.getElementById(id);

    if (tankLive == 1) {
        if (locX < -70 || locY > myHeight) {
            locX = (Math.random() * (myWidth + 300)) + 100;
            locY = -150;
        } else {
            locX = locX - (0.76 * speed);
            locY = locY + (1.71 * speed);
        }
        moveObjTo(obj, locX, locY);
    }
}

//Simple move function
function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}