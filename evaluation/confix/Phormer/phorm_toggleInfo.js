function toggleInfo(wut) {
	if ((!wut) || (wut == ''))
		wut = dg('hin').innerHTML;
	if (wut == 'Show') {
		dg('hin').innerHTML = 'Hide&nbsp;';
		dg('photoBoxes').style.display = 'block';
		dg('theImage').style.cssFloat = 'left';
		dg('theImage').style.styleFloat = 'left';
		dg('theImage').style.marginRight = '15px';
		setCookie('hideinfo', 'false');
	}
	else {
		dg('hin').innerHTML = 'Show';
		dg('photoBoxes').style.display = 'none';
		dg('theImage').style.cssFloat = 'none';
		dg('theImage').style.styleFloat = 'none';
		dg('theImage').style.marginRight = '55px';
		setCookie('hideinfo', 'true');
	}
}

function dg(x) {
	return document.getElementById(x);
}

function setCookie(key, val) {
	newd = new Date;
	newd.setMonth(newd.getMonth()+6);
	document.cookie = key+"="+val+";expires=" + newd.toGMTString();
}