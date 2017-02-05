//Fire a new meteor (if available) periodically
function fireMeteor(maxMeteors, meteorStatus, isPaused, meteorX, meteorY, meteorIteration, myWidth, angleRad, meteorVelocityX, meteorVelocityY, meteorVelocity, myLevel, meteorPeriod) {
    var i = 0;
    var launchMe = 0;
    for (i = 1; i <= maxMeteors; i++) {
        //try and find a launch slot
        if (meteorStatus[i] != 1) {
            launchMe = i;
        }
    }
    if (launchMe > 0 && isPaused == 0) {
        meteorStatus[launchMe] = 1;
        meteorX[launchMe] = (Math.random() * (myWidth + 300)) + 100;
        meteorY[launchMe] = -20
        meteorIteration[launchMe] = 1;
        angleRad = (135 + (Math.random() * 90)) * (Math.PI / 180);
        meteorVelocityX[launchMe] = (meteorVelocity + myLevel) * Math.sin(angleRad);
        meteorVelocityY[launchMe] = (meteorVelocity + myLevel) * Math.cos(angleRad);
        maintainMeteor(launchMe);
    }
    window.setTimeout("fireMeteor();", meteorPeriod);
}

function maintainMeteor(x) {}

/*
 * Types: 
 * maxMeteors: int
 * meteorStatus: [int]
 * isPaused: int
 * meteorX: [float]
 * meteorY: [float]
 * meteorIteration: [int]
 * myWidth: int
 * angleRad: float
 * meteorVelocityX: [float]
 * meteorVelocityY: [float]
 * meteorVelocity: int
 * myLevel: int
 * meteorPeriod: int
 */
