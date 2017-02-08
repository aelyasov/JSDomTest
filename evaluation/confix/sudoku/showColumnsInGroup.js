function showColumnsInGroup(level) {
    var object = document.getElementById('sudoku');
    var cellsRevealed = new Array();
    var numberArray = new Array();
    var groupCountArray = new Array();
    var maxInGroup = 5;
    if (level <= 0) level = 1;
    if (level == 1) maxInGroup = 4;
    for (var no = 0; no < countSquares[level]; no++) {
        do {
            var row = Math.floor(Math.random() * 9);
            var col = Math.floor(Math.random() * 9);
            var obj = document.getElementById('square_' + row + '_' + col);
            var parentID = obj.parentNode.id;
            var span = obj.getElementsByTagName('SPAN')[0];
            if (!numberArray[span.innerHTML]) numberArray[span.innerHTML] = 0;
            if (!groupCountArray[parentID]) groupCountArray[parentID] = 0;
        } while (cellsRevealed[row + '_' + col] || numberArray[span.innerHTML] > (3 + Math.ceil(level / 2)) || groupCountArray[parentID] >= maxInGroup);
        cellsRevealed[row + '_' + col] = true;
        if (!numberArray[span.innerHTML]) numberArray[span.innerHTML] = 0;
        numberArray[span.innerHTML]++;
        groupCountArray[parentID]++;
        showCell(obj);

    }
}


/*
 * Types: 
 * level: int
 * countSquares
 */

var countSquares = [36, 36, 34, 32, 31, 30]

function showCell(inputDiv) {
    var span = inputDiv.getElementsByTagName('SPAN')[0];
    span.style.display = '';
    inputDiv.style.backgroundColor = '#DDD';
    span.style.color = '#317082';
    var typingSpan = inputDiv.getElementsByTagName('SPAN')[1];
    typingSpan.style.display = 'none';

}