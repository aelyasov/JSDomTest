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

function do_draw(mx, my, lastmx, lastmy, reach, w, h) {
    log(1);
    if (mx < 0) {
	log(2);
        return;
    }
    log(3);
    if ((lastmx == mx) && (lastmy == my)) {
	log(4);
        if (reach < 100) {
	    log(5);
            reach++;
        }
	log(6);
    } else {
	log(7);
        lastmx = mx;
        lastmy = my;
        reach = 50;
    }
    log(8);
    var x1 = mx - reach;
    var x2 = mx + reach;
    var y1 = my - reach;
    var y2 = my + reach;
    if (x1 < 0) {
	log(9);
        x1 = 0;
    }
    log(10);
    if (y1 < 0) {
	log(11);
        y1 = 0;
    }
    log(12);
    if (x1 > w - 1) {
	log(13);
        x1 = w - 1;
    }
    log(14);
    if (y1 > h - 1) {
	log(15);
        y1 = h - 1;
    }
    log(16);
    if (x2 > w) {
	log(17);
        x2 = w;
    }
    log(18);
    if (y2 > h) {
	log(19);
        y2 = h;
    }
    console.log(x2, x1);
    log(20);
    if (x2 < x1 + 1) {
	log(21);
        x2 = x1 + 1;
    }
    log(22);
    if (y2 < y1 + 1) {
	log(23);
        y2 = y1 + 1;
    }
    log(24);
}


// all branches: (1,2)*, (1,3)*, (3,4)*, (4,5)*, (4,6)*, (3,7)*, (8,9)*, (8,10)*, (10,11)*, (10,12)*, (12,13)*, (12,14)*, (14,15)*, (14,16), (16,17)*, (16,18)*, (18,19)*, (18,20)*, (20,21), (20,22)*, (22,23), (22,24)*

// // cover branch (1,3), (3,4), (4,5), (8,9), (10,11), (12,13), (14,15), (16,17), (18,19), (20,22), (22,24); path = [1,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,22,24]
// runTest(do_draw, dom1, [0,0,0,0,0,0,0]);

// // cover branch (1,3), (3,4), (4,6), (8,9), (10,11), (12,13), (14,15), (16,17), (18,19), (20,22), (22,24); path = [1,3,4,6,8,9,10,11,12,13,14,15,16,17,18,19,20,22,24]
// runTest(do_draw, dom1, [0,0,0,0,100,0,0]);

// // cover branch (1,2); path = [1,2]
// runTest(do_draw, dom1, [-1,0,0,0,100,0,0]);

// // cover branch (1,3), (3,7), (8,9), (10,11), (12,13), (14,15), (16,17), (18,19), (20,22), (22,24); path = [1,3,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,24]
// runTest(do_draw, dom1, [1,0,0,0,100,0,0]);

// // cover branch (1,3), (3,4), (4,5), (8,10), (10,11), (12,13), (14,15), (16,17), (18,19), (20,22), (22,24); path = [1,3,4,5,6,8,10,11,12,13,14,15,16,17,18,19,20,22,24]
// runTest(do_draw, dom1, [10,0,10,0,0,0,0]);

// // cover branch (1,3), (3,4), (4,5), (8,9), (10,12), (12,14), (14,16), (16,17), (18,20), (20,22), (22,24); path = [1,3,4,5,6,8,9,10,12,14,16,18,20,22,24]
// runTest(do_draw, dom1, [0,10,0,10,0,20,20]);

// // cover branch (1,3), (3,4), (4,5), (8,9), (10,12), (12,14), (14,16), (16,17), (18,20), (20,22), (22,24); path = [1,3,4,5,6,8,9,10,12,14,16,18,20,22,24]
// runTest(do_draw, dom1, [0,0,0,0,0,0,0]);

runTest(do_draw, dom1, [0,0,0,0,-1,1,1]);

//TODO: two branches to cover
