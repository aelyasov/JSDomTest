function createTerrain() {
    //Set up global stuff
    var initialHeight = parseFloat(document.getElementById("height").value);
    var theCoords = [initialHeight, initialHeight];
    var extremity = parseFloat(document.getElementById("extremity").value);
    var detail = parseInt(document.getElementById("detail").value);
    var xsize = parseInt(document.getElementById("xsize").value);
    var ysize = parseInt(document.getElementById("ysize").value);
    var mountain = document.getElementById("mv").selectedIndex;
    if (mountain == 1) {
        var displacement = 1.0;
    } else {
        var displacement = -1.0;
    }
    var IntervalCounter = 0;

    var canvas = document.getElementById('landscape');
    canvas.height = ysize;
    canvas.width = xsize;
    // use getContext to use the canvas for drawing
    var ctx = canvas.getContext('2d');
    var lineargradient = ctx.createLinearGradient(xsize / 2, ysize / 2, xsize / 2, ysize);
    var color1 = "rgb(" + Math.floor(Math.random() * 255) + "," + Math.floor(Math.random() * 255) + "," + Math.floor(Math.random() * 255) + ")"
    var color2 = "rgb(" + Math.floor(Math.random() * 255) + "," + Math.floor(Math.random() * 255) + "," + Math.floor(Math.random() * 255) + ")"
    lineargradient.addColorStop(0, color1);
    lineargradient.addColorStop(1, color2);

    var animInterval = setInterval("animatedDraw()", 100);
}