function setTest(color, scheme) {

    function rect(c, x, y, w, h) {
        c.beginPath();
        c.rect(x, y, w, h);
        c.fill();
    }

    var t = document.getElementById("txt");
    var ctx = document.getElementById("swatch").getContext("2d");
    var c = document.getElementById("sbg").selectedIndex;
    if (scheme > 0) {
        if (arguments.length == 1) {
            var other = arguments[0];
            if (document.getElementById("fg").selectedIndex == document.getElementById("bg").selectedIndex) {
                document.getElementById(other).selectedIndex = (scheme == 1) ?
                    1 - document.getElementById(other).selectedIndex : (document.getElementById(other).selectedIndex + 1) % 3;
            }
        }
        t.style.color = colorStr(color[document.getElementById("fg").selectedIndex]);
        t.style.backgroundColor = colorStr(
            color[document.getElementById("bg").selectedIndex]);
        ctx.fillStyle = colorStr(color[c]);
        rect(ctx, 0, 0, 150, 50);
        ctx.fill();
        if (scheme == 1) {
            ctx.fillStyle = colorStr(color[1 - c]);
            rect(ctx, 50, 10, 50, 30);
        } else {
            ctx.fillStyle = colorStr(color[(c + 1) % 3]);
            rect(ctx, 20, 10, 50, 30);
            ctx.fillStyle = colorStr(color[(c + 2) % 3]);
            rect(ctx, 90, 10, 50, 30);
        }
    }
}

function colorStr(c) {
    return "rgb(" +
        Math.floor(c[0]) + "," + Math.floor(c[1]) + "," + Math.floor(c[2]) +
        ")";
}

/*
 * Types:
 * color: [int]
 * scheme: int
 */