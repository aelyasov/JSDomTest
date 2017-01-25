function reshuffle() {
	var maxRand = 400-75;
	var n = dg('thumbscount').value;
	for (var i=0; i<n; i++) {
		dg('ThumbInBox'+i).style.top = rand(maxRand)+'px';
		dg('ThumbInBox'+i).style.left = rand(maxRand)+'px';
	}
}

function dg(x) {
	return document.getElementById(x);
}

function rand(x) {
	return Math.round(Math.random()*x);
}