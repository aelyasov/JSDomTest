function start() {
    F = $("final");
    D = $("debug");
    go(get_hash(window.location.href));
    F.onmousedown = function() {
        go(0)
    }
    document.getElementById("p").onclick = function() {
        document.getElementById("p").href = F.toDataURL("image/png")
    };
}

function $(id) {
    return document.getElementById(id)
}

function get_hash(url) {
    var c = url.split("#");
    if (c.length == 2) return c[1];
    return ""
}

var F,D;

