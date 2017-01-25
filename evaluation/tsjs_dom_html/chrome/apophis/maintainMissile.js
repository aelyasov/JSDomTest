//Move missiles and track for collision (or leaving screen)
function maintainMissile(missileNo)
  {
  if (isPaused == 1)
    {
    window.setTimeout("maintainMissile(" + missileNo + ");", 200);
    return;
    }
  missileY[missileNo] = missileY[missileNo] - 10;
  document.getElementById('BULLET' + missileNo).style.top = missileY[missileNo];
  document.getElementById('BULLET' + missileNo).style.left = missileX[missileNo] - 6;
  missileIteration[missileNo]++;
  if (missileIteration[missileNo] == 5)
    {
    missileIteration[missileNo] = 1;
    }
  document.getElementById('FLAME' + missileNo).src = "img/flame" + missileIteration[missileNo] + ".png";
  document.getElementById('FLAME' + missileNo).style.top = missileY[missileNo] + 24;
  document.getElementById('FLAME' + missileNo).style.left = missileX[missileNo] - 6;

  //Look for collisions with meteors
  var i = 0;
  var hitMe = 0;
  for (i = 1; i <= maxMeteors; i++)
    {
    hitMe = collide('BULLET' + missileNo, 'METEOR' + i);
    if (hitMe > 0)
      {
      missileStatus[missileNo] = 0;
      document.getElementById('FLAME' + missileNo).style.top = -500;
      document.getElementById('BULLET' + missileNo).style.top = -500;
      document.getElementById('METEOR' + i).style.top = -500;
      explosion(meteorX[i], meteorY[i]);
      meteorStatus[i] = 0;
      myScore = myScore + 50 + myLevel;
      myKills++;
      if (myKills == 12)
        {
        myKills = 0;
        myLevel++;
        }
      writeText();
      }
    }
  if (missileY[missileNo] < -40 || missileStatus[missileNo] == 0)
    {
    missileStatus[missileNo] = 0;
    }
  else
    {
    window.setTimeout("maintainMissile(" + missileNo + ");", 20);
    }
  }

var missileY, missileX, missileIteration, missileStatus, maxMeteors;

//Check two objects for collision
function collide(obj1, obj2)
  {
  var l1 = 0;  var l2 = 0;  var r1 = 0;  var r2 = 0;  var t1 = 0;  var t2 = 0;  var b1 = 0;  var b2 = 0;
  l1 = parseInt(document.getElementById(obj1).style.left) + 3;
  r1 = parseInt(document.getElementById(obj1).style.left) + parseInt(document.getElementById(obj1).width) - 3;
  l2 = parseInt(document.getElementById(obj2).style.left) + 3;
  r2 = parseInt(document.getElementById(obj2).style.left) + parseInt(document.getElementById(obj2).width) - 3;
  t1 = parseInt(document.getElementById(obj1).style.top) + 3;
  b1 = parseInt(document.getElementById(obj1).style.top) + parseInt(document.getElementById(obj1).height) - 3;
  t2 = parseInt(document.getElementById(obj2).style.top) + 3;
  b2 = parseInt(document.getElementById(obj2).style.top) + parseInt(document.getElementById(obj1).height) - 3;

  if (r1 > l2 && l1 < r2 && t1 < b2 && b1 > t2)
    {
    return 1;
    }
  else
    {
    return 0;
    }
  }

