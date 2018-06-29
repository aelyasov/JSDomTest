//Write text variables (score, level, lives) to screen
function writeText() {
    //Do Level
    //Int to Str
    var stringText = myLevel + '';
    //Pad string
    var i = 0;
    for (i = 1; i <= 2; i++) {
        if (stringText.length < 2) {
            stringText = '0' + stringText;
        }
    }
    for (i = 1; i <= 2; i++) {
        document.getElementById('LEVEL' + i).src = "img/numbers" + stringText.charAt(i - 1) + ".png";
    }
    //Do Score
    //Int to Str
    var stringText = myScore + '';
    //Pad string
    for (i = 1; i <= 5; i++) {
        if (stringText.length < 5) {
            stringText = '0' + stringText;
        }
    }
    for (i = 1; i <= 5; i++) {
        document.getElementById('SCORE' + i).src = "img/numbers" + stringText.charAt(i - 1) + ".png";
    }
    //Do Lives
    //Int to Str
    var stringText = myLives + '';
    //Pad string
    for (i = 1; i <= 2; i++) {
        if (stringText.length < 2) {
            stringText = '0' + stringText;
        }
    }
    for (i = 1; i <= 2; i++) {
        document.getElementById('LIVES' + i).src = "img/numbers" + stringText.charAt(i - 1) + ".png";
    }
}