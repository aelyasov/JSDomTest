function getPossibleNumbers(inputObj) {

    var noArray = new Array();
    var countNumbers = 0;
    var spans = inputObj.getElementsByTagName('SPAN');
    if (spans[0].innerHTML == spans[1].innerHTML) return 0;

    var parentDiv = inputObj.parentNode;
    var subDivs = parentDiv.getElementsByTagName('DIV');
    for (var no = 0; no < subDivs.length; no++) {
        if (subDivs[no] != inputObj) {
            var spans = subDivs[no].getElementsByTagName('SPAN');
            if (spans[0].innerHTML == spans[1].innerHTML || subDivs[no].style.backgroundColor.length > 1) {
                if (!noArray[spans[0].innerHTML]) {
                    noArray[spans[0].innerHTML] = true;
                    countNumbers++;
                }
            }
        }
    }

    var numbers = inputObj.id.split('_');
    var row = numbers[1];
    var col = numbers[2];

    for (var no = 0; no < 9; no++) {

        var obj = document.getElementById('square_' + row + '_' + no);
        if (obj != inputObj) {
            var spans = obj.getElementsByTagName('SPAN');
            if (spans[0].innerHTML == spans[1].innerHTML || !spans[0].style.display) {
                if (!noArray[spans[0].innerHTML]) {
                    noArray[spans[0].innerHTML] = true;
                    countNumbers++;
                }
            }
        }

        var obj = document.getElementById('square_' + no + '_' + col);
        if (obj != inputObj) {
            var spans = obj.getElementsByTagName('SPAN');
            if (spans[0].innerHTML == spans[1].innerHTML || !spans[0].style.display) {
                if (!noArray[spans[0].innerHTML]) {
                    noArray[spans[0].innerHTML] = true;
                    countNumbers++;
                }
            }
        }
    }

    return countNumbers;
}

/*
 * Types: 
 * inputObj: Element
 */
