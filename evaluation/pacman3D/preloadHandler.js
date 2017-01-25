function preloadHandler() {
  imgCnt++;
  var d=getElement('progressbar');
  var v = imgCnt/imgTotal;
  d.style.width=Math.round(160*v)+'px';
  d=getElement('progressvalue');
  d.innerHTML=Math.round(v*100)+'%';
  if (imgCnt==imgTotal) setTimeout('preloadComplete()', 10);
}

var imgCnt, imgTotal;

function getElement(id) {
  if (document.getElementById) return document.getElementById(id);
  if (document.all) return document.all[id];
  return null;
}
