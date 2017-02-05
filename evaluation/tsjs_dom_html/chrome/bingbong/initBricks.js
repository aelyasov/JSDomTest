//Set up new brick array
function initBricks(i, myCenterH, brickV, brickH, brickStat, theLevelArt, levelData) {
    //Which row/col are we on?
    var brickRow = 0;
    var brickCol = 0
    var thisBrick = '';
    var brickLeft = myCenterH - 420;
    if (i <= 10) {
        brickRow = 70;
        brickCol = brickLeft + ((i - 1) * 85);
    } else if (i > 10 && i <= 20) {
        brickRow = 125;
        brickCol = brickLeft + ((i - 11) * 85);
    } else if (i > 20 && i <= 30) {
        brickRow = 180;
        brickCol = brickLeft + ((i - 21) * 85);
    } else if (i > 30 && i <= 40) {
        brickRow = 235;
        brickCol = brickLeft + ((i - 31) * 85);
    } else {
        brickRow = 290;
        brickCol = brickLeft + ((i - 41) * 85);
    }
    //Add appropriate brick (or nothing) to array
    thisBrick = levelData[theLevelArt].charAt(i - 1);
    if (thisBrick == 'x') {
        //Make visible
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 1;
        document.getElementById('BRICK' + i).src = "img/brick.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else if (thisBrick == 'O') {
        //Make indestructible
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 2;
        document.getElementById('BRICK' + i).src = "img/solid.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else if (thisBrick == 'Z') {
        //Make death brick
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 6;
        document.getElementById('BRICK' + i).src = "img/death.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else if (thisBrick == 'o') {
        //Make weak brick
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 3;
        document.getElementById('BRICK' + i).src = "img/weakbrick1.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else {
        //assume nothing is there
        brickStat[i] = 0;
        //Make invisible
        document.getElementById('BRICK' + i).style.left = -200;
        document.getElementById('BRICK' + i).style.top = -100;
    }
    //Do the next one?
    i++;
    if (i <= 50) {
        window.setTimeout("initBricks(" + i + ");", 20);
    }
}


/*
 * i: int
 * myCenterH: int
 * brickH: [int]
 * brickV: [int]
 * brickStat: [int]
 * levelData: [int]
 * theLevelArt: int
 */