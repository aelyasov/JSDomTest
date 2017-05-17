/*t dom:int:int:int:int:int:int */
function test(iteration, tankLive, meteorLive, tankH, myCenterH, myCenterV) {
    if (iteration == 1) {
        tankLive = 0;
        meteorLive = 0;
        tankH = -50;
        document.getElementById('TANK1').style.left = -50;
        document.getElementById('GETREADY').style.left = myCenterH - 97;
        document.getElementById('GETREADY').style.top = myCenterV - 16;
        window.setTimeout("getReady(2);", 1000);
    } else {
        if (iteration == 2) {
            tankLive = 1;
            window.setTimeout("getReady(3);", 500);
        } else {
            document.getElementById('GETREADY').style.top = -500;
            meteorLive = 1;
        }
    }
}