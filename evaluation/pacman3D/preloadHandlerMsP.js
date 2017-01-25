function preloadHandlerMsP() {
  imgCnt++;
  var d=getElement('progressbar');
  var v = imgCnt/imgTotal;
  d.style.width=Math.round(160*v)+'px';
  d=getElement('progressvalue');
  d.innerHTML=Math.round(v*100)+'%';
  if (imgCnt==imgTotal) setTimeout('preloadCompleteMsP()', 10);
}