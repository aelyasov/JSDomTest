/*t dom:int:int */
function test(number1, number2) {
    var squareObjects = document.getElementsByTagName("DIV");
    for (var counter = 0; counter < 2; counter++) {
        //var number1 = Math.ceil(Math.random() * 9);
        //var number2 = Math.ceil(Math.random() * 9);
        while (number2 == number1) {
            number2 = Math.ceil(Math.random() * 9);
        }

        var tmpObjects1 = new Array();
        var tmpObjects2 = new Array();

        for (var no = 0; no < squareObjects.length; no++) {
            var txtObj = squareObjects[no].getElementsByTagName('SPAN')[0];
            if (txtObj.innerHTML == number1) {
		tmpObjects1.push(txtObj);
	    }
            if (txtObj.innerHTML == number2) {
		tmpObjects2.push(txtObj);
	    }
        }

        // for (var n1 = 0; no < tmpObjects1.length; no++) {
        //     tmpObjects1[n1].innerHTML = number2;
        //     tmpObjects2[n1].innerHTML = number1;
        // }
    }
}
