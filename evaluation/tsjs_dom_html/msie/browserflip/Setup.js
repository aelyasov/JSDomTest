function Setup() {

    board = document.getElementById('theboard');
    game = document.getElementById('game');
    playagain = document.getElementById('playagain');
    fps = document.getElementById('fps');
    time = document.getElementById('time');

    if (window.addEventListener) {
        window.addEventListener("resize", CreateBoard, false);
        document.addEventListener("keypress", OnKeyPress, false);
    }
    else if (window.attachEvent) {
        window.attachEvent("onresize", CreateBoard);
        document.attachEvent("onkeypress", OnKeyPress);
    }

    CreateBoard();

    setInterval("Draw()", 15);

}