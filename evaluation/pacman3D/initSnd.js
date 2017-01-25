function initSnd() {
  var sliderbg;
  if (document.getElementById) {
    sliderElement=document.getElementById('sndslider');
    sliderbg=document.getElementById('sndsliderbg');
  }
  else {
    sliderElement=document.all.sndslider;
    sliderbg=document.all.sndsliderbg;
  }
  if (sliderElement) {
    setSoundVolume(sndIntialVolume);
    registerEvent(sliderElement, 'mousedown', sliderDown);
    if (sliderbg) registerEvent(sliderbg, 'mousedown', sliderBgDown);
  }
}