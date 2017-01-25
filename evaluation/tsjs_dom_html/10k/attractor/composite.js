function composite(idm, idc, idd) {
    var dst = $(idd),dc = cc(dst),width = dst.width,height = dst.height;

    var md = cc($(idm)).getImageData(0, 0, width, height);
    var cd = cc($(idc)).getImageData(0, 0, width, height);
    var dd = dc.getImageData(0, 0, width, height);

    var a,x,y,start = 0;
    for (x = 0; x < width; x++)
        for (y = 0; y < height; y++) {
            start = (x + y * width) * 4;
            a = md.data[start + 3] / 255;
            dd.data[start] = a * cd.data[start];
            dd.data[start + 1] = a * cd.data[start + 1];
            dd.data[start + 2] = a * cd.data[start + 2];
            dd.data[start + 3] = 255;
        }

    dc.putImageData(dd, 0, 0);
}


function $(id) {
    return document.getElementById(id)
}

function cc(e) {
    return e.getContext("2d")
}