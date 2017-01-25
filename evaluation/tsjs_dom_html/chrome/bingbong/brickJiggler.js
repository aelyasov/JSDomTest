//Jiggler function
function brickJiggler(whichOne, iteration, speed)
  {
  if (brickStat[whichOne] == 1)
    {
    iteration++;
    var tempTop = brickV[whichOne] + (Math.sin((iteration) / 6) * 2);
    document.getElementById('BRICK' + whichOne).style.top = tempTop;
    var tempLeft = brickH[whichOne] + (Math.cos((iteration) / 7) * 2);
    document.getElementById('BRICK' + whichOne).style.left = tempLeft;
    }
  window.setTimeout("brickJiggler(" + whichOne + "," + iteration + "," + speed + ");", speed);
  }

var brickStat;
var brickH;
var brickV;