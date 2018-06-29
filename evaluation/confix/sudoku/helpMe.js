function helpMe() {
    if (gameFinished) return false;

    if (confirm('Do you want me to reveal a number for you?')) {
        var allreadyRevealed = true;
        var counter = 0;
        do {
            var row = Math.floor(Math.random() * 9);
            var col = Math.floor(Math.random() * 9);

            var el = document.getElementById('square_' + row + '_' + col);

            var spans = el.getElementsByTagName('SPAN');
            if (spans[1].innerHTML.length == 0) {
                spans[1].innerHTML = spans[0].innerHTML;
                spans[1].style.color = '#FF0000';
                allreadyRevealed = false;
            }
            if (el.style.backgroundColor) allreadyRevealed = true;
            counter++
        } while (allreadyRevealed && counter < 500);
    }

    isGameFinished();

}