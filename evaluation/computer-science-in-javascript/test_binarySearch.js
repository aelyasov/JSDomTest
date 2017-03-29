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

function binarySearch(items, value){

    var startIndex  = 0,
        stopIndex   = items.length - 1,
        middle      = Math.floor((stopIndex + startIndex)/2);

    log(1);
    while(items[middle] != value && startIndex < stopIndex){
    
        //adjust search area
	log(2);
        if (value < items[middle]){
	    log(3);
            stopIndex = middle - 1;            
        } else {
	    // if (value > items[middle]){
	    log(4);
		startIndex = middle + 1;
	    // } else {
	    // 	log("test");
	    // }
        }
	log(5);
        
        //recalculate middle
        middle = Math.floor((stopIndex + startIndex)/2);
	log(stopIndex, startIndex, middle);
    }
    log(6);

    //make sure it's the right value
    return (items[middle] != value) ? -1 : middle;
}

/* branhes: (1,2), (1,6), (1,2), (2,3), (2,4) */

// // cover branch (1,2), (2,3); path = [1,2,3,5,6]
// runTest(binarySearch, dom1, [[1,2,3], 1]);

// // cover branch (1,6); path = [1,6]
// runTest(binarySearch, dom1, [[], 1]);

// // cover branch (1,2), (2,4),; path = [1,2,4,5,6]
// runTest(binarySearch, dom1, [[5,5], 6]);

runTest(binarySearch, dom1, [[1,2,3,4,5], 5]);
