function generate_parameters(maxtrials) {
    var i,a,b,c,d,e,f,g,h,s1,s2,s3,c1,c2,c3,r;

    for (i = 0; i < maxtrials; ++i) {
        a = rr(-2.0, 2.0);
        b = rr(-2.0, 2.0);
        c = rr(-2.0, 2.0);
        d = rr(0.2, 2.0);
        e = rr(-2.0, 2.0);
        f = rr(-2.0, 2.0);
        g = rr(-2.0, 2.0);
        h = rr(0.1, 2.0);

        var res = test_attractor(a, b, c, d, e, f, g, h, 2, 100, 1000);
        if (res.ok) break;
    }

    if (i == maxtrials) {
        a = -1.32,b = -1.47,c = 1.65,d = 1.26,e = -0.35,f = 0.63,g = 1.51,h = 1.71;
    }

    var width = height = 300;

    var zoom = Math.min(width / (res.maxx - res.minx), height / (res.maxy - res.miny));
    var dx = 0.5 * (width - (res.maxx + res.minx));
    var dy = 0.5 * (height - (res.maxy + res.miny));

    if (zoom > 1)
        zoom = 1;
    else
        zoom *= 0.9;

    zoom *= 1.5;

    s1 = 0.25;
    s2 = 0.5;
    s3 = 0.75;

    c1 = Math.floor(Math.random() * 0xffffff);
    c2 = Math.floor(Math.random() * 0xffffff);
    c3 = Math.floor(Math.random() * 0xffffff);

    r = 2 * Math.PI * Math.random();

    var parameters = {
        'a':a, 'b':b, 'c':c, 'd':d, 'e':e, 'f':f, 'g':g, 'h':h,
        's1':s1, 's2':s2, 's3':s3,
        'c1':c1, 'c2':c2, 'c3':c3,
        'r':r,
        'zoom':zoom, 'dx':dx, 'dy':dy
    }

    return parameters;
}

function rr(minx, maxx) {
    return minx + (maxx - minx) * Math.random()
}

function test_attractor(a, b, c, d, e, f, g, h, epsilon, threshold, iterations) {
    var i,j,x = y = 0,minx = miny = 9999999999,maxx = maxy = 0,seeds = [],px,py,tmpX,tmpY,unique,dx,dy;
    for (i = 0; i < iterations; i++) {
        px = Math.round(150 + x * 50);
        py = Math.round(150 + y * 50);

        tmpX = a * Math.sin(b * y) + c * Math.cos(d * x);
        tmpY = e * Math.sin(f * x) + g * Math.cos(h * y);

        x = tmpX;
        y = tmpY;

        if (seeds.length == 0) seeds.push([px,py]);
        else {
            unique = 1;
            for (j = 0; j < seeds.length; j++) {
                dx = Math.abs(seeds[j][0] - px);
                dy = Math.abs(seeds[j][1] - py);
                if (dx < epsilon && dy < epsilon) unique = 0;
            }
            if (unique) {
                seeds.push([px,py]);
                if (px < minx) minx = px;
                else if (px > maxx) maxx = px;
                if (py < miny) miny = py;
                else if (py > maxy) maxy = py;
            }
        }
    }
    return {'ok':seeds.length > threshold, 'minx':minx, 'maxx':maxx, 'miny':miny, 'maxy':maxy};
}