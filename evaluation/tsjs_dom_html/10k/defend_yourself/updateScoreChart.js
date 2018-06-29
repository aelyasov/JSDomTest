function updateScoreChart(highScores) {
    highScores.sort(function(a, b) {
        return(b - a)
    });
    var c = document.getElementById('highscores');
    if (c.hasChildNodes()) {
        while (c.childNodes.length >= 1) {
            c.removeChild(c.firstChild)
        }
    }
    for (var i = 0; i < highScores.length && i < 5; i++) {
        var d = document.createElement('li');
        d.innerHTML = highScores[i];
        c.appendChild(d)
    }
    if (highScores.length === 0) {
        var d = document.createElement('li');
        d.innerHTML = 'No high scores yet.';
        c.appendChild(d)
    }
}

/* Types:
 * highScores: [int]
 */