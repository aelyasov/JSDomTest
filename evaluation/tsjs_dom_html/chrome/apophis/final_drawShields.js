/*t dom:[int] */
function test(shieldStat) {
    for (var i = 1; i <= 5; i++) {
        if (shieldStat[i] > 0) {
            document.getElementById('SHIELD' + i).src = "img/shield" + shieldStat[i] + ".png";
        } else {
            document.getElementById('SHIELD' + i).style.top = -500;
        }
    }
}