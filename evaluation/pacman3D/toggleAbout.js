function toggleAbout() {
  if (document.images) {
    var t1=getUiElement('about1');
    var t2=getUiElement('about2');
    var img=document.images.disclosureTriangle;
    if (aboutToggle) {
      if (t1) t1.style.display='none';
      if (t2) t2.style.display='none';
      img.src=uiImgeRef.disclosure_closed.src;
      img.alt=img.title='expand';
      aboutToggle=false;
    }
    else {
      if (t1) t1.style.display='block';
      if (t2) t2.style.display='block';
      img.src=uiImgeRef.disclosure_open.src;
      img.alt=img.title='collapse';
      aboutToggle=true;
    }	
  }
}

function getUiElement(id) {
  if (document.getElementById) return document.getElementById(id);
  if (document.all) return document.all[id];
  return null;
}