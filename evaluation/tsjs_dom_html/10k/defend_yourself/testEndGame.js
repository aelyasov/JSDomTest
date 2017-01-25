function testEndGame(balls, gameWidth, gameHeight) {
    var x = gameWidth / 2;
    var y = gameHeight - 35;
    var a = 50;
    for (var i = 0; i < balls.length; i++) {
        var b = balls[i].x - x;
        var c = balls[i].y - y;
        var r = balls[i].ra + a;
        if (balls[i].y > gameHeight - 60) {
            continue
        }
        if ((b * b + c * c) < r * r) {
            return true
        }
    }
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

/*
 * Types:
 * balls: [Ball]
 * gameWidth: int
 * gameHeight: int
 */