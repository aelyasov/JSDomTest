function collide(a, b) {
    var c = 1;
    if (b.ra == 10) {
        c = 5
    }
    var d = a.x - a.vx - b.x;
    var e = a.y - a.vy - b.y;
    if (Math.sqrt(d * d + e * e) > a.ra + b.ra + 5) {
        return
    }
    d /= Math.sqrt(d * d + e * e);
    e /= Math.sqrt(d * d + e * e);
    var f = a.vx * d + a.vy * e;
    var g = b.vx * d + b.vy * e;
    var h = (2.0 * (f - g)) / (a.ra * 5 + b.ra * c);
    a.vx = a.vx - h * b.ra * d * c;
    a.vy = a.vy - h * b.ra * e * c;
    b.vx = -(b.vx - h * a.ra * d);
    b.vy = -(b.vy - h * a.ra * e);
    b.move()
}

var Ball = function(x, y, a, b, c, d, e) {
    this.x = x;
    this.y = y;
    this.ra = a;
    this.vx = b;
    this.vy = c;
    this.stroke = d;
    this.fill = e
};

/* Types:
 * a: Ball
 * b: Ball
 */