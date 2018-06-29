function decorateMenu(menu) {
    var items = menu.getElementsByTagName("li");
    for (var i = 0; i < items.length; i++) {
        items[i].firstChild.myIndex = i;
        // retain any existing onclick handlers from menu-config.xml
        if (items[i].firstChild.onclick) {
            items[i].firstChild.onclick = function() {
                eval(items[this.myIndex].firstChild.getAttribute("onclick"));
                setCookie("menuSelected", this.myIndex);
            };
        } else {
            items[i].firstChild.onclick = function() {
                setCookie("menuSelected", this.myIndex);
            };
        }
    }
    activateMenu(items);
}