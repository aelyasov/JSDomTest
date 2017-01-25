function do_draw() {
    if (m.x < 0) {
        return;
    }
    if ((lastmx == m.x) && (lastmy == m.y)) {
        if (reach < 100) {
            reach++;
        }
    } else {
        lastmx = m.x;
        lastmy = m.y;
        reach = 50;
    }
    var x1 = m.x - reach;
    var x2 = m.x + reach;
    var y1 = m.y - reach;
    var y2 = m.y + reach;
    if (x1 < 0) {
        x1 = 0;
    }
    if (y1 < 0) {
        y1 = 0;
    }
    if (x1 > w - 1) {
        x1 = w - 1;
    }
    if (y1 > h - 1) {
        y1 = h - 1;
    }
    if (x2 > w) {
        x2 = w;
    }
    if (y2 > h) {
        y2 = h;
    }
    if (x2 < x1 + 1) {
        x2 = x1 + 1;
    }
    if (y2 < y1 + 1) {
        y2 = y1 + 1;
    }
    var i = c.getImageData(x1, y1, x2 - x1, y2 - y1);
    modify_region(i, m.x - x1, m.y - y1);
    c.putImageData(i, x1, y1);
}

var lastmx, lastmy, reach;
var w, h, c;