function createFish(max) {

    fpsMeter.Reset();

    if (fish.length < max) {
        //add fish
        for (var i = fish.length; i < max; i++) {
            fish.push(new Fish());
        }
    } else {
        //remove fish
        fish.splice(max, fish.length - max);
    }
}