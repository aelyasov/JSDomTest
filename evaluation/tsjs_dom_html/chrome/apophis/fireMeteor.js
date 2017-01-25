//Fire a new meteor (if available) periodically
function fireMeteor()
  {
  var i = 0;
  var launchMe = 0;
  for (i = 1; i <= maxMeteors; i++)
    {
    //try and find a launch slot
    if (meteorStatus[i] != 1)
      {
      launchMe = i;
      }
    }
  if (launchMe > 0 && isPaused == 0)
    {
    meteorStatus[launchMe] = 1;
    meteorX[launchMe] = (Math.random() * (myWidth + 300)) + 100;
    meteorY[launchMe] = -20
    meteorIteration[launchMe] = 1;
    angleRad = (135 + (Math.random() * 90)) * (Math.PI / 180);
    meteorVelocityX[launchMe] = (meteorVelocity + myLevel) * Math.sin(angleRad);
    meteorVelocityY[launchMe] = (meteorVelocity + myLevel) * Math.cos(angleRad);
    maintainMeteor(launchMe);
    }
  window.setTimeout("fireMeteor();", meteorPeriod);
  }