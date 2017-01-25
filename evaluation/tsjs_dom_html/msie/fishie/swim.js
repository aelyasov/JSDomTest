function swim() {

    // Calculate next position of fish
    var nextX = x + xAngle * velocity * fpsMeter.timeDeltaS;
    var nextY = y + yAngle * velocity * fpsMeter.timeDeltaS;
    var nextZ = z + zAngle * .1 * velocity * fpsMeter.timeDeltaS;
    var nextScale = Math.abs(nextZ) / (zFar - zClose);

    // If fish is going to move off right side of screen
    if (nextX + fishW / 2 * scale > WIDTH) {
        // If angle is between 3 o'clock and 6 o'clock
        if ((angle >= 0 && angle < Math.PI / 2)) {
            angle = Math.PI - angle;
            xAngle = Math.cos(angle);
            yAngle = Math.sin(angle) * Math.random();
            flip = -flip;
        }
        // If angle is between 12 o'clock and 3 o'clock
        else if (angle > Math.PI / 2 * 3) {
            angle = angle - (angle - Math.PI / 2 * 3) * 2
            xAngle = Math.cos(angle);
            yAngle = Math.sin(angle) * Math.random();
            flip = -flip;
        }
    }

    // If fish is going to move off left side of screen
    if (nextX - fishW / 2 * scale < 0) {
        // If angle is between 6 o'clock and 9 o'clock
        if ((angle > Math.PI / 2 && angle < Math.PI)) {
            angle = Math.PI - angle;
            xAngle = Math.cos(angle);
            yAngle = Math.sin(angle) * Math.random();
            flip = -flip;
        }
        // If angle is between 9 o'clock and 12 o'clock
        else if (angle > Math.PI && angle < Math.PI / 2 * 3) {
            angle = angle + (Math.PI / 2 * 3 - angle) * 2
            xAngle = Math.cos(angle);
            yAngle = Math.sin(angle) * Math.random();
            flip = -flip;
        }
    }

    // If fish is going to move off bottom side of screen
    if (nextY + fishH / 2 * scale > HEIGHT) {
        // If angle is between 3 o'clock and 9 o'clock
        if ((angle > 0 && angle < Math.PI)) {
            angle = Math.PI * 2 - angle;
            xAngle = Math.cos(angle);
            yAngle = Math.sin(angle) * Math.random();
        }
    }

    // If fish is going to move off top side of screen
    if (nextY - fishH / 2 * scale < 0) {
        // If angle is between 9 o'clock and 3 o'clock
        if ((angle > Math.PI && angle < Math.PI * 2)) {
            angle = angle - (angle - Math.PI) * 2;
            xAngle = Math.cos(angle);
            yAngle = Math.sin(angle);
        }
    }

    // If fish is going too far (getting too small)
    if (nextZ <= zClose && zAngle < 0) {
        zAngle = -zAngle;

    }
    // If fish is getting to close (getting too large)
    if (((WIDTH / fishW) * 10) < ((fishW * fish.length) / WIDTH)) {
        zFarFactor = .3
    } else if (((WIDTH / fishW) * 2) < ((fishW * fish.length) / WIDTH)) {
        zFarFactor = .5
    } else {
        zFarFactor = 1
    }

    if (nextZ >= zFar * zFarFactor && zAngle > 0) {
        zAngle = -zAngle;

    }
    if (scale < .1) {
        scale = .1
    }; //don't let fish get too tiny

    //draw the fish
    //locate the fish
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(scale, scale); // make the fish bigger or smaller depending on how far away it is.
    ctx.transform(flip, 0, 0, 1, 0, 0); //make the fish face the way he's swimming.
    ctx.drawImage(imageStrip, fishW * cell, fishH * species, fishW, fishH, -fishW / 2, -fishH / 2, fishW, fishH); //draw the fish
    ctx.save();
    scale = nextScale // increment scale for next time
    ctx.restore();
    ctx.restore();

    //increment to next state
    x = nextX;
    y = nextY;
    z = nextZ;
    if (cell >= cellCount - 1 || cell <= 0) {
        cellReverse = cellReverse * -1;
    } //go through each cell in the animation
    cell = cell + 1 * cellReverse; //go back down once we hit the end of the animation
}