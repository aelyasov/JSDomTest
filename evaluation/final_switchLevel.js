/*t dom:bool */
function test(gameFinished) {
    var linkObj = document.getElementsByTagName("BODY")[0];
    var confirmSwitch = gameFinished;
    if (confirmSwitch) {
        var parentObj = linkObj.parentNode.parentNode;
        var links = parentObj.getElementsByTagName('A');
        for (var no = 0; no < links.length; no++) {
            links[no].style.fontWeight = 'normal';
        }
        linkObj.style.fontWeight = 'bold';
    }
}