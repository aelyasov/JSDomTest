function fixPositions(fixType) {
    var canvas = document.getElementById("canvas")

    if (fixType == undefined || fixType == "canvas") {
        canvas.style.left = window.innerWidth / 2 - canvas.width / 2 + "px";
        canvas.style.top = window.innerHeight / 2 - canvas.height / 2 + "px";
    }
    if (fixType == undefined || fixType == "menu") {
        document.getElementById("menu").style.left = window.innerWidth / 2 + canvas.width / 2 + 5 + "px";
        document.getElementById("menu").style.top = window.innerHeight / 2 - canvas.height / 2 + "px";
    }
}

/*
 * Types:
 * fixType: string
 */
