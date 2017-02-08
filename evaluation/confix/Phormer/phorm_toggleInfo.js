function toggleInfo(wut) {
    if ((!wut) || (wut == ''))
        wut = document.getElementById('hin').innerHTML;
    if (wut == 'Show') {
        document.getElementById('hin').innerHTML = 'Hide&nbsp;';
        document.getElementById('photoBoxes').style.display = 'block';
        document.getElementById('theImage').style.cssFloat = 'left';
        document.getElementById('theImage').style.styleFloat = 'left';
        document.getElementById('theImage').style.marginRight = '15px';
        setCookie('hideinfo', 'false');
    } else {
        document.getElementById('hin').innerHTML = 'Show';
        document.getElementById('photoBoxes').style.display = 'none';
        document.getElementById('theImage').style.cssFloat = 'none';
        document.getElementById('theImage').style.styleFloat = 'none';
        document.getElementById('theImage').style.marginRight = '55px';
        setCookie('hideinfo', 'true');
    }
}


function setCookie(key, val) {
    var newd = new Date;
    newd.setMonth(newd.getMonth() + 6);
    document.cookie = key + "=" + val + ";expires=" + newd.toGMTString();
}

/*
 * Types: 
 * wut: string
 */