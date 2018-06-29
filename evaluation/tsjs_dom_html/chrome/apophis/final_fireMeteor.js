/*t int:[int]:int:[int]:[int]:[int]:int:int:[int]:[int]:int:int:int */
function test(maxMeteors, meteorStatus, isPaused, meteorX, meteorY, meteorIteration, myWidth, angleRad, meteorVelocityX, meteorVelocityY, meteorVelocity, myLevel, meteorPeriod) {
    var launchMe = 0;
    for (var i = 1; i <= maxMeteors; i++) {
        if (meteorStatus[i] != 1) {
            launchMe = i;
        }
    }
    if (launchMe > 0 && isPaused == 0) {
	meteorStatus[launchMe] = 1;
        meteorX[launchMe] = (Math.random() * (myWidth + 300)) + 100;
        meteorY[launchMe] = -20;
        meteorIteration[launchMe] = 1;
        angleRad = (135 + (Math.random() * 90)) * (Math.PI / 180);
        meteorVelocityX[launchMe] = (meteorVelocity + myLevel) * Math.sin(angleRad);
        meteorVelocityY[launchMe] = (meteorVelocity + myLevel) * Math.cos(angleRad);
    }
}