/*t dom:bool */
function test(isAjaxing) {
    var v = document.getElementById('indicator').innerHTML;
    var l = v.length;
    var neck = 2;
    if (l > neck) {
        v = v.substring(0, l - 3 * 7);
    }
    if ((l % 3) == 0) {
        document.getElementById('indicator').innerHTML = '&#149;      ' + v;
    } else {
        document.getElementById('indicator').innerHTML = '&nbsp; ' + v;
    }
    if (isAjaxing) {
        setTimeout("updateIndic();", 500);
    } else {
        document.getElementById('indicator').innerHTML = '';
    }
}