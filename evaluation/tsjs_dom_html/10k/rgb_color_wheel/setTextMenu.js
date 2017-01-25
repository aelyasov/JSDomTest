function setTextMenu(id, n) {

    var prompt = [
        ["Base Color", "Complement"],
        ["Base Color", "Triad 1", "Triad 2"],
        ["Base Color", "Split 1", "Split 2"],
        ["Base Color", "Analog 1", "Analog 2"]
    ]

    var m = document.getElementById(id);
    var opt = prompt[n];
    var el;
    while (m.lastChild != null) {
        m.removeChild(m.lastChild);
    }
    for (var i = 0; i < opt.length; i++) {
        el = document.createElement("option");
        el.appendChild(document.createTextNode(opt[i]));
        m.appendChild(el);
    }
}

/*
 * Types:
 * n: int
 * id: string
 */