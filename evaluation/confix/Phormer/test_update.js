const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='ss_link2'></div><div id='ss_theend'></div><div id='ss_n'></div><div id='ss_playpause_link'></div></div><div id='ss_playpause_link2'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='ss_link2'></div><div id='ss_photo'></div><div id='ss_title'></div><div id='ss_desc'></div><div id='ss_date'></div><div id='ss_n'></div><div id='ss_link1'></div></body></html>";

let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='ss_link2'></div><div id='ss_theend'></div><div id='ss_n'></div><div id='ss_playpause_link'></div></div><div id='ss_playpause_link2'></div><div id='ss_refresh'></div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

// (int,[string],bool,[string],[string],bool,[string],[string],int)
function ss_update(ss_cur, ss_date, ss_play, ss_src, ss_pid, ss_smaller, ss_ttl, ss_desc, ss_awaits) {
    ss_cur = Math.max(ss_cur, 0);

    console.log("1");
    if (ss_cur >= ss_date.length) {
        console.log("2");
        document.getElementById('ss_link2').style.display = 'none';
        document.getElementById('ss_theend').style.display = 'block';
        ss_cur = ss_date.length;
        document.getElementById('ss_n').innerHTML = "Final";
        console.log("3");
        if (ss_play) {
            console.log("4");
            ss_play = !ss_play;
            document.getElementById('ss_playpause_link').innerHTML = (ss_play) ? 'Pause it' : 'Play it';
            document.getElementById('ss_playpause_link2').innerHTML = document.getElementById('ss_playpause_link').innerHTML;
            console.log("5");
            if (ss_awaits <= 0) {
                console.log("6");
                ss_awaits++;
                var lookup = parseInt(document.getElementById('ss_refresh').value);
            }
            console.log("7");
        }
	console.log("8");
	
    } else {
        console.log("9");
        document.getElementById('ss_theend');
        document.getElementById('ss_link2').style.display = 'none';

        var ss_loaded = (document.getElementById('ss_photo').src == ss_src[ss_cur]);

        var link = ".?p=" + ss_pid[ss_cur];
        var src = ss_src[ss_cur];
        src = ss_smaller ? src_smaller(src) : src;
	
        document.getElementById('ss_photo').src = src;
	document.getElementById('ss_title').innerHTML = ss_ttl[ss_cur];
	document.getElementById('ss_desc').innerHTML = ss_desc[ss_cur];
        document.getElementById('ss_date').innerHTML = ss_date[ss_cur];
        document.getElementById('ss_n').innerHTML = 1 + ss_cur;
        document.getElementById('ss_link1').setAttribute('href', link);
        document.getElementById('ss_link2').setAttribute('href', link);
        console.log("10");
    }
    console.log("11");
}

function src_smaller(x) {
    if (x.charAt(x.length - 1) == ")")
        x = x.substr(3, x.length - 4);
    return x.substr(0, x.length - 5) + "4.jpg";
}


// all branches: (1,2), (1,9), (3,4), (3,8), (5,6), (5,7)

// cover branch (1,2), (3,4), (5,7); path = [1,2,3,4,5,7,8,11]
runTest(ss_update, dom1, [5,[],true,[],[],true,[],[],1]);

// cover branch (1,9), (10,11); path = [1,9,10,11]
runTest(ss_update, dom2, [0,["a"],true,["a"],[],true,[],[],0]);

// cover branch (1,2), (3,8); path = [1,2,3,8,11]
runTest(ss_update, dom1, [5,[],false,[],[],true,[],[],0]);

// cover branch (1,2), (3,4), (5,6); path = [1,2,3,4,5,6,7,8,11]
runTest(ss_update, dom3, [5,[],true,[],[],true,[],[],-2]);
