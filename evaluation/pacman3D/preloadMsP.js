function preloadMsP() {
  imgCnt=0;
  imgTotal=mspacimages.length+mspacimages2.length;
  var i;
  for (i=0; i<mspacimages.length; i++) {
    var n=mspacimages[i];
    var img=imgRef[n]=new Image();
    img.src=imgPath+n+'.png';
    if (img.complete) {
      imgTotal--;
    }
    else {
      img.onload=preloadHandlerMsP;
    }
  }
  for (i=0; i<mspacimages2.length; i++) {
    var n=mspacimages2[i];
    var img=imgRef[n]=new Image();
    img.src=imgPath+n+'.gif';
    if (img.complete) {
      imgTotal--;
    }
    else {
      img.onload=preloadHandlerMsP;
    }
  }
  if (imgTotal==0) {
    preloadCompleteMsP();
  }
  else {
    var d=getElement('progress');
    setVisibility(d, true);
    d.style.display='block';
    d=getElement('progressbar');
    d.style.width='0px';
    d=getElement('progressvalue');
    d.innerHTML='0%';
  }
}