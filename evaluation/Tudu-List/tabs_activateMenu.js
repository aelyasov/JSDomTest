function activateMenu(items) {
    var activeMenu;
    var found = 0;
    for (var i = 0; i < items.length; i++) {
        var url = items[i].firstChild.getAttribute("href");
        var current = document.location.toString();
        if (current.indexOf(url) != -1) {
            found++;
        }
    }

    // more than one found, use cookies
    if (found > 1) {
        var menuSelected = getCookie("menuSelected");
        if (items[menuSelected].parentNode.className == "submenu") {
            items[menuSelected].firstChild.className = "selected";
            items[menuSelected].parentNode.parentNode.className = "selected";
        } else {
            items[menuSelected].className += "selected";
        }
    } else {
        // only one found, match on URL
        for (var i = 0; i < items.length; i++) {
            var url = items[i].firstChild.getAttribute("href");
            var current = document.location.toString();
            if (current.indexOf(url) != -1) {
                if (items[i].parentNode.className == "submenu") {
                    items[i].firstChild.className = "selected";
                    items[i].parentNode.parentNode.className = "selected";
                } else {
                    items[i].className += "selected";
                }
            }
        }
    }
}