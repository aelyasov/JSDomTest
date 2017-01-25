function init() {
    var i = document.getElementById("world");
    var n = document.getElementById("background-canvas");
    var m = document.getElementById("panels");
    var l = document.getElementById("status");
    document.getElementById("message");
    var P = document.getElementById("title");
    var Q = document.getElementById("startButton");
    if (i && i.getContext) {
        var c = i.getContext("2d");
        var C = n.getContext("2d");
        document.addEventListener("mousemove", R, false);
        document.addEventListener("mousedown", S, false);
        document.addEventListener("mouseup", T, false);
        i.addEventListener("touchstart", U, false);
        document.addEventListener("touchmove", V, false);
        document.addEventListener("touchend", W, false);
        window.addEventListener("resize", K, false);
        Q.addEventListener("click", d, false);
        document.addEventListener("keydown", h, false);
        document.addEventListener("keyup", x, false);
        var b = new Player;
        K();
        if (r) {
            document.getElementById("panel").style.display = "none";
            l.style.width = e + "px";
            i.style.border = "none";
            setInterval(M, 1E3 / 30)
        } else setInterval(M, 1E3 / s);
        var a = C.createRadialGradient(e * 0.5, f * 0.5, 0, e * 0.5, f * 0.5, 500);
        a.addColorStop(0, "rgba(0, 70, 70, 1)");
        a.addColorStop(1, "rgba(0, 8, 14, 1)");
        C.fillStyle = a;
        C.fillRect(0, 0, e, f)
    }
}

var R, S, T, U, V, W, d, h, x, r, M, s, e, f; 

function K() {
    e = r ? window.innerWidth : 900;
    f = r ? window.innerHeight : 550;
    b.position.x = e * 0.5;
    b.position.y = f * 0.5;
    i.width = e;
    i.height = f;
    n.width = e;
    n.height = f;
    var a = (window.innerWidth - e) * 0.5,
        g = (window.innerHeight - f) * 0.5;
    i.style.position = "absolute";
    i.style.left = a + "px";
    i.style.top = g + "px";
    n.style.position = "absolute";
    n.style.left = a + 6 + "px";
    n.style.top = g + 6 + "px";
    if (r) {
        m.style.left = "0px";
        m.style.top = "0px";
        l.style.left = "0px";
        l.style.top = "0px"
    } else {
        m.style.left = a + 6 + "px";
        m.style.top = g + 200 + "px";
        l.style.left = a + 6 + "px";
        l.style.top = g + 6 + "px"
    }
}

function Player() {
    this.position = {
        x: 0,
        y: 0
    };
    this.length = 15;
    this.energy = 30;
    this.energyRadiusTarget = this.energyRadius = 0;
    this.radius = 60;
    this.angle = 0;
    this.coreQuality = 16;
    this.coreNodes = []
}