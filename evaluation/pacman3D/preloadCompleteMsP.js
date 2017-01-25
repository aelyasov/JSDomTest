function preloadCompleteMsP() {
  var d=getElement('progress');
  setVisibility(d, false);
  d.style.display='none';
  mspacImgsLoaded=true;
  newGame();
}