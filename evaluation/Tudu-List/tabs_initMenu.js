function initMenu() {
    var uls = document.getElementsByTagName("ul");
    for (var i = 0; i < uls.length; i++) {
        if (uls[i].className == "menuList") {
            decorateMenu(uls[i]);
        }
    }
}