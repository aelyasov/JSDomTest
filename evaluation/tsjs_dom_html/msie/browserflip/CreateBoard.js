// Create Board

function CreateBoard() {

    images = [];
    imageBackground = [];
    imageBackgroundFinalTile = [];
    imageZoomStage = [];
    imageAnimating = [];
    imageCompleted = [];
    imageXCenter = [];
    imageYCenter = [];
    startTime = 0;
    gameOver = false;
    gameStarted = false;

    board.innerHTML = '';

    countZoomStages = zoomStages.length - 5;

    var boardWidth = (xCount * defaultRadius * 2) - (xCount * 2);
    var boardHeight = (yCount * defaultRadius * 1.5) + (defaultRadius * 0.5) - (yCount * 2);
    var spacing = (document.documentElement.clientWidth - (xCount * defaultRadius * 2)) / 2;

    board.style.width = boardWidth + "px";
    board.style.height = boardHeight + "px";
    game.style.width = boardWidth + 130 + "px";
    game.style.height = boardHeight + 120 + "px";
    playagain.style.top = (boardHeight) + 14 + "px";
    playagain.style.left = (boardWidth / 2) - 40 + "px";
    playagain.style.visibility = "hidden";


    var c = 1, n = 1;
    for (var yLocation = 1; yLocation <= yCount; yLocation++) {

        var middleLine = Math.floor(yCount / 2) + 1;
        if (yLocation < middleLine) {
            xDrawCount = xCount - (middleLine - yLocation);
        }
        else if (yLocation === middleLine) {
            xDrawCount = xCount;
        }
        else {
            xDrawCount = xCount - (yLocation - middleLine);
        }

        if (n == 1) { c = 1; n = 2; }
        else if (n == 2) { c = 3; n = 3; }
        else { c = 5; n = 1; }

        for (var xLocation = 1; xLocation <= xDrawCount; xLocation++) {

            var img = document.createElement('img');
            img.style.position = "absolute";

            img.src = TILE;
            var background = TILE;
            var finaTile = TILE;
            switch (c) {
                case 1:
                    background = IELOGO;
                    finaTile = IELOGOWATERMARKTILE;
                    c++;
                    break;
                case 2:
                    background = LOGO1;
                    finaTile = LOGO1WATERMARKTILE;
                    c++;
                    break;
                case 3:
                    background = LOGO2;
                    finaTile = LOGO2WATERMARKTILE;
                    c++;
                    break;
                case 4:
                    background = LOGO3;
                    finaTile = LOGO3WATERMARKTILE;
                    c++;
                    break;
                case 5:
                    background = LOGO4;
                    finaTile = LOGO4WATERMARKTILE;
                    c = 1;
                    break;
            }

            var xCenter, yCenter;
            yCenter = (yLocation * defaultRadius * 1.5) - (yLocation * 2) - (defaultRadius * 0.5);

            if ((yLocation % 2) > 0) {
                xCenter = ((xLocation * defaultRadius * 2) - (xLocation * 2) + (((xCount - xDrawCount) / 2) * defaultRadius * 2) - defaultRadius);
            }
            else {
                xCenter = ((xLocation * defaultRadius * 2) - (xLocation * 2) + (((xCount - xDrawCount) / 2) * defaultRadius * 2) - defaultRadius);
            }

            img.style.left = xCenter - defaultRadius + 'px';
            img.style.top = yCenter - defaultRadius + 'px';
            img.style.width = defaultRadius * 2 + 'px';
            img.style.height = defaultRadius * 2 + 'px';
            img.style.zIndex = 0;
            img.myIndex = images.length;

            if (window.addEventListener) {
                img.addEventListener("mouseover", MouseOverImage, false);
            }
            else if (window.attachEvent) {
                img.attachEvent("onmouseover", MouseOverImage, false);
            }

            board.appendChild(img);
            images.push(img);
            imageBackground.push(background);
            imageBackgroundFinalTile.push(finaTile);
            imageAnimating.push(false);
            imageCompleted.push(false);
            imageXCenter.push(xCenter);
            imageYCenter.push(yCenter);
            imageZoomStage.push(0);
        }

    }


}