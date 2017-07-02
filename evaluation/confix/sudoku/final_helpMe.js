/*t dom:int:int */
function test(row, col) {
    var allreadyRevealed = true;
    // var counter = 0;
    for (var counter = 0; counter < 5; counter++) {
        if (allreadyRevealed) {
            var el = document.getElementById('square_' + row + '_' + col);

            var spans = el.getElementsByTagName('SPAN');
            if (spans[1].innerHTML.length == 0) {
                spans[1].innerHTML = spans[0].innerHTML;
                spans[1].style.color = '#FF0000';
                allreadyRevealed = false;
            }
            if (el.style.backgroundColor) {
                allreadyRevealed = true;
            }
        } else {
            break;
        }
    }
}