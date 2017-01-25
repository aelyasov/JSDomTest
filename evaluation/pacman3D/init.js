function init() {
  elements.maze=getElement('maze');
  if ((!elements.maze) || (!document.images) || (!document.createElement)) {
    if (prompt('Sorry, DOM compatible browser (Mozilla, Firefox, Internet Explorer 5+, Safari, etc) required.\n\nTry the legacy version for older browsers?\n')) {
			self.location.replace('legacy/JS-PacManPlus.htm');
    }
    return;
  }
  setVisibility(getElement('progress'), true);
  elements.scoredisplay=getElement('scoredisplay');
  elements.hiscoredisplay=getElement('hiscoredisplay');
  elements.leveldisplay=getElement('leveldisplay');
  elements.mazedisplay=getElement('mazedisplay');
  elements.pacmodemessage=getElement('pacmodemessage');
  preload();
}

var elements = new Object();

function getElement(id) {
  if (document.getElementById) return document.getElementById(id);
  if (document.all) return document.all[id];
  return null;
}

function setVisibility(obj, v) {
  obj.style.visibility= (v)? 'visible' : 'hidden';
}

