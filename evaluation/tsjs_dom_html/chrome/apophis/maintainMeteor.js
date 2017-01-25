//Maintain and track meteor
function maintainMeteor(meteorNo)
  {
  if (isPaused == 1 || meteorLive == 0)
    {
	    window.setTimeout("maintainMeteor(" + meteorNo + ");", 200);
    }
  else
    {
    meteorY[meteorNo] = meteorY[meteorNo] - meteorVelocityY[meteorNo];
    meteorX[meteorNo] = meteorX[meteorNo] + meteorVelocityX[meteorNo];
    mWidth = document.getElementById('METEOR' + meteorNo).style.width;
    mHeight = document.getElementById('METEOR' + meteorNo).style.height;
    if (meteorStatus[meteorNo] != 0)
      {
      document.getElementById('METEOR' + meteorNo).style.top = meteorY[meteorNo] - (mWidth / 2);
      document.getElementById('METEOR' + meteorNo).style.left = meteorX[meteorNo] - (mHeight / 2);
      }
    meteorIteration[meteorNo]++;
    if (meteorIteration[meteorNo] == 4)
      {
      meteorIteration[meteorNo] = 1;
      }
    //Meteor type
    meteorType = ((meteorNo + 3) % 4) + 1;
    document.getElementById('METEOR' + meteorNo).src = "img/meteor" + meteorType + meteorIteration[meteorNo] + ".png";

    //Look for collisions with shields
    var i = 0;
    var hitMe = 0;
    for (i = 1; i <= 5; i++)
      {
      hitMe = collide('METEOR' + meteorNo, 'SHIELD' + i);
      if (hitMe == 1)
        {
        if (shieldStat[i] < 7 && shieldStat[i] > 0)
          {
          shieldStat[i]++;
          }
        else
          {
          shieldStat[i] = 0;
          }
        drawShields();
        meteorStatus[meteorNo] = 0;
        explosion(meteorX[meteorNo], meteorY[meteorNo]);
        document.getElementById('METEOR' + meteorNo).style.top = -500;
        }
      }

    //Game over if all shields are destroyed
    allDead = 1;
    for (i = 1; i <= 5; i++)
      {
      if (shieldStat[i] > 0)
        {
        allDead = 0;
        }
      }
    if (allDead == 1)
      {
      meteorStatus[meteorNo] = 0;
      gameOver();
      }

    //Look for collisions with tank
    hitMe = collide('METEOR' + meteorNo, 'TANK1');
    if (hitMe > 0)
      {
      meteorStatus[meteorNo] = 0;
      tankExplosion(tankH, 1);
      document.getElementById('METEOR' + meteorNo).style.top = -500;
      myLives--;
      writeText();
      if (myLives == 0)
        {
        gameOver();
        }
      else
        {
        getReady(1);
        }
      }

    if (meteorStatus[meteorNo] == 0 || meteorY[meteorNo] > myHeight || meteorX[meteorNo] < - 100 || meteorX[meteorNo] >= myWidth + 100)
      {
      meteorStatus[meteorNo] = 0;
      }
    else
      {
    	  window.setTimeout("maintainMeteor(" + meteorNo + ");", 80- myLevel); 
      }
    }
  }