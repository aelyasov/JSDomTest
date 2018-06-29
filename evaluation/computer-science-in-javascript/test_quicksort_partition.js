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

function partition(items, left, right) {

    var pivot   = items[Math.floor((right + left) / 2)],  // pivot value is middle item
        i       = left,     // starts from left and goes right to pivot index
        j       = right;    // starts from right and goes left to pivot index

    log(1);
    // while the two indices don't match
    while (i <= j) {
	log(2);
        // if the item on the left is less than the pivot, continue right
        while (items[i] < pivot) {
	    log(3);
            i++;
        }
	log(4);
        // if the item on the right is greater than the pivot, continue left
        while (items[j] > pivot) {
	    log(5);
	    j--;
        }
	log(6);

        // if the two indices still don't match, swap the values
        if (i <= j) {
	    log(7);
            swap(items, i, j);

            // change indices to continue loop
            i++;
            j--;
        }
	log(8);
    }
    log(9);

    // this value is necessary for recursion
    return i;
}


function swap(items, firstIndex, secondIndex){
    var temp = items[firstIndex];
    items[firstIndex] = items[secondIndex];
    items[secondIndex] = temp;
}

/* branhes: (1,2), (2,3), (2,4), (4,5), (4,6), (6,7), (6,8), (1,9) */

// cover branch (1,2), (2,4), (4,6), (6,7); path = [1,2,4,6,7,8,9]
runTest(partition, dom1, [[],0,0]);

// cover branch (1,9); path = [1,9]
runTest(partition, dom1, [[],4,1]);

// cover branch (1,2), (2,3); path = [1,2,3,3,4,6,7,8,2,4,6,7,8,9]
runTest(partition, dom1, [[1,2,3],0,4]);

// cover branch (1,2), (2,3), (4,6), (6,7), (4,5), (6,8); path = [1,2,3,3,4,6,7,8,2,4,5,6,8,9]
runTest(partition, dom1, [[1,2,3,5],0,4]);

