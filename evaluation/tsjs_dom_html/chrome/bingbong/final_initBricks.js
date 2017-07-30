/*t dom:int:[int]:[int]:[int]:[int]:int:[string]  */
function test(i, myCenterH, brickV, brickH, brickStat, theLevelArt, levelData) {
    var brickRow = 0;
    var brickCol = 0;
    var thisBrick = '';
    var brickLeft = myCenterH - 420;
    if (i <= 10) {
        brickRow = 70;
        brickCol = brickLeft + ((i - 1) * 85);
    } else {
        if (i > 10 && i <= 20) {
            brickRow = 125;
            brickCol = brickLeft + ((i - 11) * 85);
        } else {
            if (i > 20 && i <= 30) {
                brickRow = 180;
                brickCol = brickLeft + ((i - 21) * 85);
            } else {
                if (i > 30 && i <= 40) {
                    brickRow = 235;
                    brickCol = brickLeft + ((i - 31) * 85);
                } else {
                    brickRow = 290;
                    brickCol = brickLeft + ((i - 41) * 85);
                }
            }
        }
    }
    thisBrick = levelData[theLevelArt].charAt(i - 1);
    if (thisBrick == 'x') {
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 1;
        document.getElementById('BRICK' + i).src = "img/brick.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else {
        if (thisBrick == 'O') {
            brickH[i] = brickCol;
            brickV[i] = brickRow;
            brickStat[i] = 2;
            document.getElementById('BRICK' + i).src = "img/solid.png";
            document.getElementById('BRICK' + i).style.left = brickH[i];
            document.getElementById('BRICK' + i).style.top = brickV[i];
        } else {
            if (thisBrick == 'Z') {
                brickH[i] = brickCol;
                brickV[i] = brickRow;
                brickStat[i] = 6;
                document.getElementById('BRICK' + i).src = "img/death.png";
                document.getElementById('BRICK' + i).style.left = brickH[i];
                document.getElementById('BRICK' + i).style.top = brickV[i];
            } else {
                if (thisBrick == 'o') {
                    brickH[i] = brickCol;
                    brickV[i] = brickRow;
                    brickStat[i] = 3;
                    document.getElementById('BRICK' + i).src = "img/weakbrick1.png";
                    document.getElementById('BRICK' + i).style.left = brickH[i];
                    document.getElementById('BRICK' + i).style.top = brickV[i];
                } else {
                    brickStat[i] = 0;
                    document.getElementById('BRICK' + i).style.left = -200;
                    document.getElementById('BRICK' + i).style.top = -100;
                }
            }
        }
    }
    i++;
    if (i <= 50) {
        //window.setTimeout("initBricks(" + i + ");", 20);
    }
}