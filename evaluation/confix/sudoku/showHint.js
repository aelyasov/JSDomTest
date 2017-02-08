function showHint() {
    var hintDiv = document.getElementById('hintDiv');
    var hintDivInner = hintDiv.getElementsByTagName('DIV')[0];
    var maxExistingNo = 0;
    var objectToTry = false;
    for (var row = 0; row < 9; row++) {
        for (var col = 0; col < 9; col++) {
            var obj = document.getElementById('square_' + row + '_' + col);
            if (obj.style.backgroundColor) continue;
            if (!isCorrect(obj)) {
                hintDivInner.innerHTML = 'This one is wrong';
                hintDiv.style.left = getLeftPos(obj) + 'px';
                hintDiv.style.top = getTopPos(obj) - 50 + 'px';
                hintDiv.style.display = 'block';
                return;
            }

            var existingNumbers = getPossibleNumbers(obj);
            if (existingNumbers > maxExistingNo) {
                maxExistingNo = existingNumbers;
                objectToTry = obj;
            }
        }
    }

    if (objectToTry) {
        hintDivInner.innerHTML = 'Try this one ';
        hintDiv.style.left = getLeftPos(objectToTry) + 'px';
        hintDiv.style.top = getTopPos(objectToTry) - 50 + 'px';
        hintDiv.style.display = 'block';

    }
}

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

function isCorrect(divObj) {
    var spans = divObj.getElementsByTagName('SPAN');
    if (spans[0].innerHTML == spans[1].innerHTML || spans[1].innerHTML.length == 0) return true;
    return false;
}

function getLeftPos(inputObj) {
    var returnValue = inputObj.offsetLeft;
    while ((inputObj = inputObj.offsetParent) != null) returnValue += inputObj.offsetLeft;
    return returnValue;
}

function getTopPos(inputObj) {

    var returnValue = inputObj.offsetTop;
    while ((inputObj = inputObj.offsetParent) != null) {
        returnValue += inputObj.offsetTop;
    }
    return returnValue;
}