//Maintain and track meteor
function maintainMeteor(meteorNo, isPaused, meteorLive, meteorY, meteorVelocityY, meteorX, meteorVelocityX, mWidth, mHeight, meteorStatus, meteorIteration, meteorType, shieldStat, allDead) {
    if (isPaused == 1 || meteorLive == 0) {
        window.setTimeout("maintainMeteor(" + meteorNo + ");", 200);
    } else {
        meteorY[meteorNo] = meteorY[meteorNo] - meteorVelocityY[meteorNo];
        meteorX[meteorNo] = meteorX[meteorNo] + meteorVelocityX[meteorNo];
        mWidth = document.getElementById('METEOR' + meteorNo).style.width;
        mHeight = document.getElementById('METEOR' + meteorNo).style.height;
        if (meteorStatus[meteorNo] != 0) {
            document.getElementById('METEOR' + meteorNo).style.top = meteorY[meteorNo] - (mWidth / 2);
            document.getElementById('METEOR' + meteorNo).style.left = meteorX[meteorNo] - (mHeight / 2);
        }
        meteorIteration[meteorNo]++;
        if (meteorIteration[meteorNo] == 4) {
            meteorIteration[meteorNo] = 1;
        }
        //Meteor type
        meteorType = ((meteorNo + 3) % 4) + 1;
        document.getElementById('METEOR' + meteorNo).src = "img/meteor" + meteorType + meteorIteration[meteorNo] + ".png";

        //Look for collisions with shields
        var i = 0;
        var hitMe = 0;
        for (i = 1; i <= 5; i++) {
            hitMe = collide('METEOR' + meteorNo, 'SHIELD' + i);
            if (hitMe == 1) {
                if (shieldStat[i] < 7 && shieldStat[i] > 0) {
                    shieldStat[i]++;
                } else {
                    shieldStat[i] = 0;
                }
                drawShields();
                meteorStatus[meteorNo] = 0;
                explosion(meteorX[meteorNo], meteorY[meteorNo]);
                document.getElementById('METEOR' + meteorNo).style.top = -500;
            }
        }

        //Game over if all shields are destroyed
        allDead = 1;
        for (i = 1; i <= 5; i++) {
            if (shieldStat[i] > 0) {
                allDead = 0;
            }
        }
        if (allDead == 1) {
            meteorStatus[meteorNo] = 0;
            gameOver();
        }

        //Look for collisions with tank
        hitMe = collide('METEOR' + meteorNo, 'TANK1');
        if (hitMe > 0) {
            meteorStatus[meteorNo] = 0;
            tankExplosion(tankH, 1);
            document.getElementById('METEOR' + meteorNo).style.top = -500;
            myLives--;
            writeText();
            if (myLives == 0) {
                gameOver();
            } else {
                getReady(1);
            }
        }

        if (meteorStatus[meteorNo] == 0 || meteorY[meteorNo] > myHeight || meteorX[meteorNo] < -100 || meteorX[meteorNo] >= myWidth + 100) {
            meteorStatus[meteorNo] = 0;
        } else {
            window.setTimeout("maintainMeteor(" + meteorNo + ");", 80 - myLevel);
        }
    }
}

function collide(obj1, obj2) {
    var l1 = 0;
    var l2 = 0;
    var r1 = 0;
    var r2 = 0;
    var t1 = 0;
    var t2 = 0;
    var b1 = 0;
    var b2 = 0;
    l1 = parseInt(document.getElementById(obj1).style.left) + 3;
    r1 = parseInt(document.getElementById(obj1).style.left) + parseInt(document.getElementById(obj1).width) - 3;
    l2 = parseInt(document.getElementById(obj2).style.left) + 3;
    r2 = parseInt(document.getElementById(obj2).style.left) + parseInt(document.getElementById(obj2).width) - 3;
    t1 = parseInt(document.getElementById(obj1).style.top) + 3;
    b1 = parseInt(document.getElementById(obj1).style.top) + parseInt(document.getElementById(obj1).height) - 3;
    t2 = parseInt(document.getElementById(obj2).style.top) + 3;
    b2 = parseInt(document.getElementById(obj2).style.top) + parseInt(document.getElementById(obj1).height) - 3;

    if (r1 > l2 && l1 < r2 && t1 < b2 && b1 > t2) {
        return 1;
    } else {
        return 0;
    }
}

function drawShields() {
    var i = 0;
    for (i = 1; i <= 5; i++) {
        if (shieldStat[i] > 0) {
            document.getElementById('SHIELD' + i).src = "img/shield" + shieldStat[i] + ".png";
        } else {
            document.getElementById('SHIELD' + i).style.top = -500
        }
    }
}

//Tank explosion
function tankExplosion(explodeX, iteration) {
    if (iteration == 8) {
        moveObjTo(TANKEXPLODE, explodeX - 37, -500);
    } else {
        moveObjTo(TANKEXPLODE, explodeX - 37, myHeight - 75);
        document.getElementById('TANKEXPLODE').src = "img/tank-explode" + iteration + ".png";
    }
    iteration++;
    if (iteration < 9) {
        window.setTimeout("tankExplosion(" + explodeX + "," + iteration + ");", 250);
    }
}

//Simple move function
function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

//Create an explosion
function explosion(explodeX, explodeY) {
    var i = 0;
    var launchMe = 0;
    for (i = 1; i <= maxExplosions; i++) {
        //try and find a launch slot
        if (explosionStat[i] == 0) {
            launchMe = i;
        }
    }
    if (launchMe > 0 && isPaused == 0) {
        explosionStat[launchMe] = 1;
        explosionX[launchMe] = explodeX;
        explosionY[launchMe] = explodeY;
        window.setTimeout("maintainExplosion(" + launchMe + ");", 50);
    }
}

/*
 * Types:
 * launchMe: int
 * isPaused: int
 * meteorLive: int
 * meteorY: [float]
 * meteorVelocityY: [float]
 * meteorX: [float]
 * meteorVelocityX: [float]
 * mWidth: int
 * mHeight: int
 * meteorStatus: [int]
 * meteorIteration: [int]
 * meteorType: int
 * shieldStat: [int]
 */