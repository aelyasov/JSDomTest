function ss_update(ss_cur, ss_date, ss_play, ss_loaded, ss_src, link, ss_pid, ss_smaller, ss_ttl, ss_desc) {
    ss_cur = Math.max(ss_cur, 0);

    if (ss_cur >= ss_date.length) {
        hideElem('ss_link2');
        showElem('ss_theend');
        ss_cur = ss_date.length;
        dg('ss_n').innerHTML = "Final";
        if (ss_play)
            ss_playpause();
    } else {
        hideElem('ss_theend');
        inlineElem('ss_link2');

        ss_loaded = (dg('ss_photo').src == ss_src[ss_cur]);

        link = ".?p=" + ss_pid[ss_cur];
        var src = ss_src[ss_cur];
        src = ss_smaller ? src_smaller(src) : src;

        dg('ss_photo').src = src;
        dg('ss_date').innerHTML = ss_date[ss_cur];
        dg('ss_title').innerHTML = ss_ttl[ss_cur];
        dg('ss_desc').innerHTML = ss_desc[ss_cur];
        dg('ss_n').innerHTML = 1 + ss_cur;
        dg('ss_link1').setAttribute('href', link);
        dg('ss_link2').setAttribute('href', link);
        if (ss_cur < ss_date.length) {
            preimg = new Image;
            preimg.src = ss_src[ss_cur + 1];
        }
    }
}

/*
 * Types:
 * ss_date: [string]
 * ss_cur: int
 * ss_play: boolean
 * ss_loaded: boolean
 * ss_src: [string]
 * link: string
 * ss_pid: [string]
 * ss_smaller: boolean
 * ss_ttl: [string]
 * ss_desc: [string]
 */


function hideElem(x) {
    try {
        dg(x).style.display = 'none';
    } catch (e) {}
}

function showElem(x) {
    try {
        dg(x).style.display = 'block';
    } catch (e) {}
}

function dg(x) {
    return document.getElementById(x);
}

function inlineElem(x) {
    try {
        dg(x).style.display = 'inline';
    } catch (e) {}
}

function ss_playpause() {
    ss_play = !ss_play;
    dg('ss_playpause_link').innerHTML = (ss_play) ? 'Pause it' : 'Play it';
    dg('ss_playpause_link2').innerHTML = dg('ss_playpause_link').innerHTML;
    ss_run();
}

function src_smaller(x) {
    if (x.charAt(x.length - 1) == ")")
        x = x.substr(3, x.length - 4);
    return x.substr(0, x.length - 5) + "4.jpg";
}