function insertNumber(e, higlightedCell, gameFinished) {
    var code;

    document.getElementById('hintDiv').style.display = 'none';

    if (document.all) e = event;
    if (!higlightedCell) return;
    if (gameFinished) return;
    if (e.keyCode) code = e.keyCode;
    else if (e.which) code = e.which;
    var span = higlightedCell.getElementsByTagName('SPAN')[1];

    var numbers = higlightedCell.id.split('_');

    var row = numbers[1] / 1;
    var col = numbers[2] / 1;
    var nextObject = false;

    if (code == 39) { // Right arrow
        if (col < 8) {
            nextObject = document.getElementById('square_' + row + '_' + (col / 1 + 1));
            if (nextObject.style.backgroundColor) {
                while (col < 8 && nextObject.style.backgroundColor) {
                    col = col + 1;
                    nextObject = document.getElementById('square_' + row + '_' + col);
                }
            }
        }
    }
    if (code == 37) { // Left arrow
        if (col > 0) {
            nextObject = document.getElementById('square_' + row + '_' + (col / 1 - 1));
            if (nextObject.style.backgroundColor) {
                while (col > 0 && nextObject.style.backgroundColor) {
                    col = col - 1;
                    nextObject = document.getElementById('square_' + row + '_' + col);
                }
            }
            if (nextObject.style.backgroundColor) nextObject = false;
        }
    }
    if (code == 38) {
        if (row > 0) {
            nextObject = document.getElementById('square_' + (row - 1) + '_' + col);
            if (nextObject.style.backgroundColor) {
                while (row > 0 && nextObject.style.backgroundColor) {
                    row = row - 1;
                    nextObject = document.getElementById('square_' + row + '_' + col);
                }
            }
        }
    }
    if (code == 40) {
        if (row < 8) {
            nextObject = document.getElementById('square_' + (row + 1) + '_' + col);
            if (nextObject.style.backgroundColor) {
                while (row < 8 && nextObject.style.backgroundColor) {
                    row = row + 1;
                    nextObject = document.getElementById('square_' + row + '_' + col);
                }
            }
        }
    }

    if (nextObject) {
        highlightSquare(false, nextObject);
    }

    if (code == 46 || code == 8) { // Delete
        span.innerHTML = '';
        if (code == 8) return false;
    }
    if (code > 96 && code <= 105) code -= 48;
    if (code > 48 && code <= 57) {
        var theChar = String.fromCharCode(code);
        span.innerHTML = theChar;
    }

    isGameFinished();
}

/*
 * Types:
 * e: Event
 * higlightedCell: Element
 * gameFinished: boolean
 * code: int
 */

