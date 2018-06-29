/*t dom */
function test() {
    var maxRand = 400 - 75;
    var n = document.getElementById('thumbscount').value;
    for (var i = 0; i < n; i++) {
        document.getElementById('ThumbInBox' + i).style.top = rand(maxRand) + 'px';
        document.getElementById('ThumbInBox' + i).style.left = rand(maxRand) + 'px';
    }
}

function rand(x) {
    return Math.round(Math.random() * x);
}