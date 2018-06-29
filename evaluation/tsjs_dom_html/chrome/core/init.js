function init(r, e, s, f) {

    function R(a) {}
    function S(a) {}
    function T(a) {}
    function U(a) {}
    function V(a) {}
    function W(a) {}
    function K(a) {}
    function d(a) {}
    function h(a) {}
    function x(a) {}
    function M(a) {}
    function K(a) {}

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
        // var b = new Player; // commented because b is not used
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


/*
 * r: boolean
 * e: int
 * s: int
 * f: int
 */
