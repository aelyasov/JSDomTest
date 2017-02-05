//Kill a ball (wherever it is right now)
function killBall(iteration) {
    if (iteration == 1) {
        moveObjTo(BALL, -100, -100);
        EXPLODE.src = "img/explode1.png";
        moveObjTo(EXPLODE, ballH - 1, ballV - 1);
        window.("killBall(2);", 80);
    } else if (iteration == 2) {
        EXPLODE.src = "img/explode2.png";
        window.setTimeout("killBall(3);", 80);
    } else if (iteration == 3) {
        EXPLODE.src = "img/explode3.png";
        window.setTimeout("killBall(4);", 80);
    } else if (iteration == 4) {
        moveObjTo(EXPLODE, -100, -100);
        window.setTimeout("killBall(5);", 160);
    } else {
        freezeBall(1);
        initBall();
        moveBall();
    }
}

/*
 * Types:
 * 
 */
