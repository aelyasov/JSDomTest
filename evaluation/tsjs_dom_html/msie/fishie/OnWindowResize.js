function OnWindowResize(e) {
    var bodyWidth = window.innerWidth;
    var bodyHeight = window.innerHeight;

    if (typeof e == 'undefined')
        e = window.event;
    //on resize reset the WIDTH, HEIGHT and canvas sizes
    WIDTH = bodyWidth;
    HEIGHT = bodyHeight;
    document.getElementById('canvas1').width = WIDTH;
    document.getElementById('canvas1').height = HEIGHT;
    document.getElementById('background').width = WIDTH;
    document.getElementById('background').height = HEIGHT;
    //redraw the background
    ctx3.clearRect(0, 0, WIDTH, HEIGHT);
    drawBackground();
}