function compute(sixth, segment, rad, lighten, hWidth) {
    var rgb = new Array(3);
    var limit = (lighten) ? 255 : 0;
    var colorIncr = new Array(255, -1, 0, 0, 1, 255);

    for (var i = 0; i < 3; i++) {
        rgb[i] = colorIncr[(sixth + i * 4) % 6];
        if (rgb[i] == -1) {
            rgb[i] = 255 - segment * 32;
        } else if (rgb[i] == 1) {
            rgb[i] = segment * 32;
        }
        rgb[i] = limit + (rgb[i] - limit) * (rad / hWidth);
    }
    return rgb;
}

/*
 * Types:
 * sixth: int
 * segment: int
 * segment: int
 * lighten: int
 * hWidth: int
 */
