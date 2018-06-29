function switchLevel(initLevel, linkObj, gameFinished) {
    var confirmSwitch = gameFinished;
    if (!confirmSwitch) confirmSwitch = confirm('Click OK to finish the current game');
    if (confirmSwitch) {
        var parentObj = linkObj.parentNode.parentNode;
        var links = parentObj.getElementsByTagName('A');
        for (var no = 0; no < links.length; no++) {
            links[no].style.fontWeight = 'normal';
        }
        linkObj.style.fontWeight = 'bold';
        level = initLevel;
        setTimeout('initSudoku()', 20);
    }
}

var level;

/*
 * Types:
 * initLevel: int
 * linkObj: Node
 * gameFinished: boolean
 */