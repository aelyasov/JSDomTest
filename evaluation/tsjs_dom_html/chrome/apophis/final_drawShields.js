/*t dom:[int] */
function test(shieldStat) {
    // modified loop upper bound
    for (var i = 0; i <=5; i++) {
        if (shieldStat[i] > 0) {
            document.getElementById('SHIELD' + i).src = "img/shield" + shieldStat[i] + ".png";
        } else {
            document.getElementById('SHIELD' + i).style.top = -500;
        }
    }
}