function precompute(src, dst, w, h) {
    var x, y, tot = i = 0;
    for (y = 0; y < h; y++)
        for (x = 0; x < w; x++) {
            tot = src.data[4 * i + 3];
            if (x > 0) tot += dst[(i - 1) * 4 + 3];
            if (y > 0) tot += dst[(i - w) * 4 + 3];
            if (x > 0 && y > 0) tot -= dst[(i - w - 1) * 4 + 3];
            dst[4 * i + 3] = tot;
            i++;
        }
}