// Core drawing routine which animates the board

function Draw() {

    if (gameOver == false) {

        var moreAnimation = false;
        for (var i = 0; i < images.length; i++) {

            if (imageCompleted[i] == false) {
                moreAnimation = true;
            }

            if (imageAnimating[i] === true) {

                var change = zoomStages[imageZoomStage[i]];
                var newRadius = (defaultRadius + change) > 0 ? (defaultRadius + change) : 0;

                if (change < -28) {
                    images[i].src = imageBackground[i];
                }

                if (warpSpeed == true) {
                    newRadius = newRadius * 3;
                    images[i].src = imageBackground[i];
                }

                images[i].style.left = (imageXCenter[i] - newRadius) + "px";
                images[i].style.top = (imageYCenter[i] - newRadius) + "px";
                images[i].style.width = (newRadius * 2) + "px";
                images[i].style.height = (newRadius * 2) + "px";
                images[i].style.zIndex = change;

                if (warpSpeed == true) {
                    imageZoomStage[i] = imageZoomStage[i] + 10;
                }
                else {
                    imageZoomStage[i] = imageZoomStage[i] + 3;
                }

                if (imageZoomStage[i] >= countZoomStages) {
                    images[i].src = imageBackgroundFinalTile[i];
                    imageZoomStage[i] = 0;
                    imageAnimating[i] = false;
                    imageCompleted[i] = true;

                    images[i].style.left = (imageXCenter[i] - defaultRadius) + "px";
                    images[i].style.top = (imageYCenter[i] - defaultRadius) + "px";
                    images[i].style.width = defaultRadius * 2 + 'px';
                    images[i].style.height = defaultRadius * 2 + 'px';
                }
            }
        }

        if (gameStarted) {

            var rightNow = new Date().getTime();
            if (Math.floor(rightNow/1000) == Math.floor(currentSecond/1000) ) {
                currentFPS++;
            }
            else {
                currentSecond = rightNow;
                fps.innerHTML = (currentFPS > 60) ? 60 : currentFPS;
                currentFPS = 1;
            }

            var delta = rightNow - startTime;
            time.innerHTML = Math.floor(delta/1000) + "." + Math.floor((delta % 1000) / 10);

            if (!moreAnimation) {
                GameOver();
            }
        }
    }
}