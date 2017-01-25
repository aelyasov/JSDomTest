//Kill a brick
function killBrick(brickNo, iteration)
  {
  if (iteration == 1)
    {
    EXPLODE.src = "img/explode1.png";
    moveObjTo(EXPLODE, brickH[brickNo] + 20, brickV[brickNo] + 4);
    document.getElementById('BRICK' + brickNo).style.left = -100;
    window.setTimeout("killBrick(0,2);", 80);
    }
  else if (iteration == 2)
    {
    EXPLODE.src = "img/explode2.png";
    window.setTimeout("killBrick(0,3);", 80);
    }
  else if (iteration == 3)
    {
    EXPLODE.src = "img/explode3.png";
    window.setTimeout("killBrick(0,4);", 80);
    }
  else if (iteration == 4)
    {
    moveObjTo(EXPLODE, -100, -100);
    }
  }