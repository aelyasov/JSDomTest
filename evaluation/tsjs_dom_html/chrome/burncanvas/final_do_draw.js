/*t int:int:int:int:int:int:int */
function test(mx, my, lastmx, lastmy, reach, w, h) {
    if (mx < 0) {
        return;
    }
    if ((lastmx == mx) && (lastmy == my)) {
        if (reach < 100) {
            reach++;
        }
    } else {
        lastmx = mx;
        lastmy = my;
        reach = 50;
    }
    var x1 = mx - reach;
    var x2 = mx + reach;
    var y1 = my - reach;
    var y2 = my + reach;
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
}