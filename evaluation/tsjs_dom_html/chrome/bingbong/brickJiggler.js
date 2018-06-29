//Jiggler function
function brickJiggler(whichOne, iteration, speed, brickStat, brickH, brickV) {
    if (brickStat[whichOne] == 1) {
        iteration++;
        var tempTop = brickV[whichOne] + (Math.sin((iteration) / 6) * 2);
        document.getElementById('BRICK' + whichOne).style.top = tempTop;
        var tempLeft = brickH[whichOne] + (Math.cos((iteration) / 7) * 2);
        document.getElementById('BRICK' + whichOne).style.left = tempLeft;
    }
    window.setTimeout("brickJiggler(" + whichOne + "," + iteration + "," + speed + ");", speed);
}

/*
 * Types: 
 * whichOne: int
 * iteration: int
 * speed: float
 * brickStat: [int]
 * brickV: [int]
 * brickH: [int]
 * brickStat: [int]
 */