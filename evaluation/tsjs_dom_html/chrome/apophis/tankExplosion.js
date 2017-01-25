//Tank explosion
function tankExplosion(explodeX, iteration)
  {
  if (iteration == 8)
    {
    moveObjTo(TANKEXPLODE, explodeX - 37, -500);
    }
  else
    {
    moveObjTo(TANKEXPLODE, explodeX - 37, myHeight - 75);
    document.getElementById('TANKEXPLODE').src = "img/tank-explode" + iteration + ".png";
    }
  iteration++;
  if (iteration < 9)
    {
	    window.setTimeout("tankExplosion(" + explodeX + "," + iteration + ");", 250);
    }
  }