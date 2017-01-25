function updateIndic() {
	var v = dg('indicator').innerHTML;
	var l = v.length;
	var neck = 52;
	if (l > neck)
		v = v.substring(0, l-3*7)
	if ((l%3) == 0)
		dg('indicator').innerHTML = '&#149;      '+v;
	else
		dg('indicator').innerHTML = '&nbsp; '+v;
	if (isAjaxing)
		setTimeout("updateIndic();", 500);
	else
		dg('indicator').innerHTML = '';
}

var isAjaxing = false;

function dg(x) {
	return document.getElementById(x);
}