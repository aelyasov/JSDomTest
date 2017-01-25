function get_hash(url) {
    var c = url.split("#");
    if (c.length == 2) return c[1];
    return ""
}