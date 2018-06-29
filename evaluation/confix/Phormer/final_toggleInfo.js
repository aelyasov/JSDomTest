/*t dom:string */
function test(wut) {
    if ((!wut) || (wut == '')) {
        wut = document.getElementById('hin').innerHTML;
    }
    if (wut == 'Show') {
        document.getElementById('hin').innerHTML = 'Hide&nbsp;';
        document.getElementById('photoBoxes').style.display = 'block';
        document.getElementById('theImage').style.cssFloat = 'left';
        document.getElementById('theImage').style.styleFloat = 'left';
        document.getElementById('theImage').style.marginRight = '15px';
    } else {
        document.getElementById('hin').innerHTML = 'Show';
        document.getElementById('photoBoxes').style.display = 'none';
        document.getElementById('theImage').style.cssFloat = 'none';
        document.getElementById('theImage').style.styleFloat = 'none';
        document.getElementById('theImage').style.marginRight = '55px';
    }
}