/*t dom:int:[string]:bool:[string]:[string]:bool:[string]:[string]:int */
function test(ss_cur, ss_date, ss_play, ss_src, ss_pid, ss_smaller, ss_ttl, ss_desc, ss_awaits) {
    ss_cur = Math.max(ss_cur, 0);

    if (ss_cur >= ss_date.length) {
        document.getElementById('ss_link2').style.display = 'none';
        document.getElementById('ss_theend').style.display = 'block';
        ss_cur = ss_date.length;
        document.getElementById('ss_n').innerHTML = "Final";
        if (ss_play) {
            ss_play = !ss_play;
            document.getElementById('ss_playpause_link').innerHTML = (ss_play) ? 'Pause it' : 'Play it';
            document.getElementById('ss_playpause_link2').innerHTML = document.getElementById('ss_playpause_link').innerHTML;
            if (ss_awaits <= 0) {
                ss_awaits++;
                var lookup = parseInt(document.getElementById('ss_refresh').value);
            }
        }
	
    } else {
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
    }
}

function src_smaller(x) {
    if (x.charAt(x.length - 1) == ")")
        x = x.substr(3, x.length - 4);
    return x.substr(0, x.length - 5) + "4.jpg";
}
