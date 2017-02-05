//Move ball!
function moveBall(isPaused) {
    var BALL = document.getElementById("BALL")
    ballAlive = 1;
    moveObjTo(BALL, ballH, ballV);
    //Move it
    if (isPaused == 0 && isSuspended == 0) {
        firstSteps++;
        ballVelocityV = ballVelocityV - 0.1;
        if (firstSteps <= 50) {
            ballH = ballH + (ballVelocityH / 5);
            ballV = ballV - (ballVelocityV / 5);
        } else {
            ballH = ballH + ballVelocityH;
            ballV = ballV - ballVelocityV;
        }
    }

    //Collision with paddle?
    if (ballV >= (myHeight - 64)) {
        if (ballH >= (paddleH - 15) && ballH <= (paddleH + 104) && ballVelocityV < 0) {
            var pingH = ballH - 10;
            var pingV = ballV;
            goPing(pingH, pingV, 1);
            ballVelocityV = 0 - ballVelocityV;
            theScore++;
            writeScore();
            //Tweak ball velocity dependent on strike location and paddle strength
            ballVelocityV = ballVelocityV + ((paddlePower - 50) / 25);

            howFarIn = ballH - (paddleH - 15);
            if (howFarIn < 40) {
                howFarIn = 40 - howFarIn;
                ballVelocityH = ballVelocityH - (howFarIn / 10);
            } else if (howFarIn > 79) {
                howFarIn = howFarIn - 79;
                ballVelocityH = ballVelocityH + (howFarIn / 10);
            }
        }
    }

    //Collision with wall?
    if (ballH <= 0) {
        //Hit left wall
        ballVelocityH = 0 - ballVelocityH;
    }
    if (ballH >= (myWidth - 30)) {
        //Hit right wall
        ballVelocityH = 0 - ballVelocityH;
    }
    if (ballV <= 0) {
        //Hit top wall
        ballVelocityV = 0 - ballVelocityV;
    }

    //Bottom wall - Dead!
    if (ballV >= (myHeight - 30)) {
        ballAlive = 0;
        killBall(1);
        return;
    }

    //Collision with brick?
    //Brick goes from brickH[],brickV[]-brickH[]+73,brickV[]+41
    //Ball goes from ballH,ballV-ballH+29,ballV+30
    //Allow only one hit per cycle
    var hitMe = 0;
    var i = 0;
    for (i = 1; i <= 50; i++) {
        if (brickStat[i] >= 1) {
            if (ballH + 29 >= brickH[i] && ballH <= brickH[i] + 73 && ballV <= brickV[i] + 41 && ballV + 29 >= brickV[i]) {
                //What side did we hit?
                //Calculate distances from different edges
                bottomEdge = Math.abs(ballV - (brickV[i] + 41));
                topEdge = Math.abs((ballV + 29) - brickV[i]);
                rightEdge = Math.abs(ballH - (brickH[i] + 73));
                leftEdge = Math.abs((ballH + 29) - brickH[i]);
                //Reflect appropriately from the edge we're closest to...
                if (hitMe == 0) {
                    if (bottomEdge < topEdge && bottomEdge < rightEdge && bottomEdge < leftEdge) {
                        //Hit bottom
                        ballVelocityV = 0 - ballVelocityV;
                    } else if (topEdge < bottomEdge && topEdge < rightEdge && topEdge < leftEdge) {
                        //Hit top
                        ballVelocityV = 0 - ballVelocityV;
                    } else if (rightEdge < topEdge && rightEdge < topEdge && rightEdge < leftEdge) {
                        //Hit right
                        ballVelocityH = 0 - ballVelocityH;
                    } else {
                        //Hit left
                        ballVelocityH = 0 - ballVelocityH;
                    }
                }
                //Kill brick (if it's a brick that we hit)!
                if (brickStat[i] == 3) {
                    brickStat[i] = 4;
                    document.getElementById('BRICK' + i).src = "img/weakbrick2.png";
                } else if (brickStat[i] == 4) {
                    brickStat[i] = 5;
                    document.getElementById('BRICK' + i).src = "img/weakbrick3.png";
                } else if (brickStat[i] == 6) {
                    ballAlive = 0;
                    killBall(1);
                    return;
                } else if (brickStat[i] == 1 || brickStat[i] == 5) {
                    hitMe = 1;
                    brickStat[i] = 0;
                    theScore = theScore + 10 + theLevel;
                    writeScore();
                    killBrick(i, 1);
                }
                //Are all the bricks dead?
                var foundOne = 0;
                var j = 0;
                for (j = 1; j <= 50; j++) {
                    if (brickStat[j] == 1 || brickStat[j] == 3 || brickStat[j] == 4 || brickStat[j] == 5) {
                        foundOne++
                    }
                }
                if (foundOne == 0) {
                    //Good work!
                    theLevel++;
                    theLevelArt++;
                    if (theLevelArt > numLevels) {
                        //We've drawn all the levels; start over
                        theLevelArt = 1;
                    }
                    drawLevel(1);
                    freezeBall(1);
                    initBall();
                    initBricks(1);
                }
            }
        }
    }

    //Keep going!
    window.setTimeout("moveBall();", 10);
}

//Simple move function
function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

/*
 * isPaused: int
 */