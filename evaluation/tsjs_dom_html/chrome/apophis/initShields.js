//Reset shields
function initShields()
  {
  shieldStat[1] = 1;
  shieldStat[2] = 1;
  shieldStat[3] = 1;
  shieldStat[4] = 1;
  shieldStat[5] = 1;
  var i = 0;
  for (i = 1; i <= 5; i++)
    {
    shieldH[i] = (((myWidth - 200) / 5) * (i - 1)) + 150
    document.getElementById('SHIELD' + i).src = "img/shield1.png";
    document.getElementById('SHIELD' + i).style.top = myHeight - 100;
    document.getElementById('SHIELD' + i).style.left = shieldH[i];
    }
  }