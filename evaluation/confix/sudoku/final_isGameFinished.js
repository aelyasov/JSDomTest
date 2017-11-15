/*t dom */ 
function test() {
    var obj = document.getElementById('sudoku');
    var subDivs = obj.getElementsByTagName('DIV');
    var allOk = true;
    for (var no = 0; no < subDivs.length; no++) {
        if (subDivs[no].className.indexOf('sudokuSquare') >= 0 && !subDivs[no].style.backgroundColor) {
            var spans = subDivs[no].getElementsByTagName('SPAN');
            if (spans[0].innerHTML != spans[1].innerHTML) {
                allOk = false;
                break;
            }
        }
    }
    if (allOk) {
        console.log('Congratulations! You did it');
    }
}
