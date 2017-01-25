function endGame( timeStart, totalBalls, totalBullets) {
    document.getElementById('score').style.display = 'block';
    var a = (new Date().getTime()) - timeStart;
    document.getElementById('time').innerHTML = Math.floor(a / 1000);
    document.getElementById('numballs').innerHTML = totalBalls;
    document.getElementById('numbullets').innerHTML = totalBullets;
    totalBullets = totalBullets ? totalBullets : 1;
    var b = Math.ceil(a * (totalBalls / totalBullets));
    document.getElementById('final').innerHTML = Math.ceil(a * (totalBalls / totalBullets));
    saveHighScore(b)
}

function saveHighScore(a) {
    highScores.push(a);
    if (window.localStorage) {
        window.localStorage.setItem('high_scores', highScores.join(','))
    }
    updateScoreChart()
}

var highScores

function updateScoreChart() {
    highScores.sort(function(a, b) {
        return(b - a)
    });
    var c = $('highscores');
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
 * timeStart: Date
 * totalBalls: int
 * totalBullets: int
 */