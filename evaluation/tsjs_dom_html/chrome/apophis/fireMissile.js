//Fire new missile (if available)
function fireMissile(maxMissiles, isPaused, tankLive, missileStatus, tankH, myHeight, missileX, missileY, missileIteration) {
    var i = 0;
    var launchMe = 0;
    for (i = 1; i <= maxMissiles; i++) {
        //try and find a launch slot
        if (missileStatus[i] != 1) {
            launchMe = i;
        }
    }
    if (launchMe > 0 && isPaused == 0 && tankLive == 1) {
        missileStatus[launchMe] = 1;
        missileX[launchMe] = tankH;
        missileY[launchMe] = myHeight - 55;
        missileIteration[launchMe] = 1;
        maintainMissile(launchMe);
    }
}

function maintainMissile(x) {}

/*
 * Types:
 * maxMissiles: int
 * isPaused: int
 * tankLive: int
 * missileStatus: [int]
 * tankH: float
 * myHeight: int
 * missileX: [float]
 * missileY: [float]
 * missileIteration: [int]
 */