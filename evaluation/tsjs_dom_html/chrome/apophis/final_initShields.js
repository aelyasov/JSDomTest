/*t dom:[int]:int:int */
function test(shieldH, myWidth, myHeight) {
    for (var i = 1; i <= 5; i++) {
        shieldH[i] = (((myWidth - 200) / 5) * (i - 1)) + 150;
        document.getElementById('SHIELD' + i).src = "img/shield1.png";
        document.getElementById('SHIELD' + i).style.top = myHeight - 100;
        document.getElementById('SHIELD' + i).style.left = shieldH[i];
    }

}