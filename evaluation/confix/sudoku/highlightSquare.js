function highlightSquare(e, inputObj) {
    document.getElementById('hintDiv').style.display = 'none';
    if (!inputObj) inputObj = this;
    if (inputObj.style.backgroundColor) return;
    if (gameFinished) return;
    inputObj.className = 'sudokuSquareHighlighted';
    if (higlightedCell && higlightedCell != inputObj) higlightedCell.className = 'sudokuSquare';
    higlightedCell = inputObj;
    if (document.all) inputObj.focus();

}