const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function quickSort(items, left, right) {

    var index;
    log(1);
    if (items.length > 1) {
	log(2);
        left = typeof left != "number" ? 0 : left;
        right = typeof right != "number" ? items.length - 1 : right;

        index = partition(items, left, right);

        if (left < index - 1) {
	    log(3);
            quickSort(items, left, index - 1);
        }
	log(4);

        if (index < right) {
	    log(5);
	    quickSort(items, index, right);
        }
	log(6);
    }
    log(7);

    return items;
}


function swap(items, firstIndex, secondIndex){
    var temp = items[firstIndex];
    items[firstIndex] = items[secondIndex];
    items[secondIndex] = temp;
}

function partition(items, left, right) {

    var pivot   = items[Math.floor((right + left) / 2)],  
        i       = left,     
        j       = right;  

    while (i <= j) {

        while (items[i] < pivot) {
            i++;
        }

        while (items[j] > pivot) {
            j--;
        }

        if (i <= j) {
            swap(items, i, j);

            i++;
            j--;
        }
    }

    return i;
}

/* branhes: (1,2), (1,7), (2,3), (2,4), (4,5), (4,6) */

// cover branch (1,7); path = [1,7]
runTest(quickSort, dom1, [[],0,0]);

// cover branch (1,2), (2,4), (4,6); path = [1,2,4,6,7]
runTest(quickSort, dom1, [[1,2],0,0]);

// cover branch (1,2), (2,4), (4,6); path = [1,2,4,6,7]
runTest(quickSort, dom1, [[1,4,2],0,2]);
