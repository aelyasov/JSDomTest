function preloadComplete() {
  var d=getElement('progress');
  setVisibility(d, false);
  d.style.display='none';
  initMaze();
  initSprites();
  setVisibility(getElement('controls'), true);
  readPreferences();
  var select=getElement('mazeselect');
  select.selectedIndex=0;
  for (var i=0; i<select.options.length; i++) {
    if (select.options[i].value==mazeMode) {
      select.selectedIndex=i;
      break;
    }
	}
  select=getElement('pacmodeselect');
  select.selectedIndex=0;
  for (i=0; i<select.options.length; i++) {
    if (select.options[i].value==settingsPacmode) {
      select.selectedIndex=i;
      break;
    }
  }
  setQuality(settingsQualityValue);
  setSpeed(settingsSpeedValue);
  setPacMode(settingsPacmode, false);
  setMazeMode(mazeMode, false);
  enableKeyboard();
  if (window.activateTouchControls) activateTouchControls();
  setTimeout('newGame()', 10);
}