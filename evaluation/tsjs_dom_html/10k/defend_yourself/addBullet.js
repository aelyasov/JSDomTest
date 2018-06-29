/*t  */
function addBullet(mouseIsDown, spacebarDown, nextBulletAt, fms, ammo, ammoUnit, gameWidth, targetX, originX, targetY, originY, bulletStroke, bulletFill, totalBullets, nextBulletIntervalEmpty, nextBulletInterval, bullets) {
    var a = false;
    if (mouseIsDown || spacebarDown) {
        if (!nextBulletAt || fms == nextBulletAt) {
            var b = targetX - originX;
            var c = targetY - originY;
            var d = Math.sqrt(b * b + c * c);
            var x = (b / d) * 10;
            var y = (c / d) * 10;
            if (spacebarDown) {
                if (ammo > ammoUnit * 50) {
                    bullets.push(new Ball(originX, originY, 10, x, y, bulletStroke, bulletFill));
                    a = true
                }
            } else {
                bullets.push(new Ball(originX, originY, 4, x, y, bulletStroke, bulletFill));
                a = true
            }
            totalBullets++;
            if (ammo === 0) {
                nextBulletAt = fms + nextBulletIntervalEmpty
            } else {
                nextBulletAt = fms + nextBulletInterval
            }
        }
        if (a && spacebarDown) {
            ammo -= ammoUnit * 50;
            spacebarDown = false
        } else if (!spacebarDown) {
            ammo -= ammoUnit
        }
        if (ammo < 0) {
            ammo = 0
        }
    } else {
        nextBulletAt = false;
        ammo += ammoUnit;
        if (ammo > gameWidth) {
            ammo = gameWidth
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

/* Types:
 * mouseIsDown: bool
 * spacebarDown: bool
 * nextBulletAt: bool
 * fms: int
 * ammo: int
 * ammoUnit: int
 * gameWidth: int
 * targetX: int
 * originX: int
 * targetY: int
 * originY: int
 * bulletStroke: string
 * bulletFill: string
 * totalBullets: int
 * bullets: [Ball]
 * nextBulletIntervalEmpty: int
 * nextBulletInterval: int
 */