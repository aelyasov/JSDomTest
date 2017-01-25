function init() {

    var canvas = document.getElementById('world');
    var paused = document.getElementById('paused');
    var intro = document.getElementById('intro');

    if (canvas && canvas.getContext) {

        // Fetch references to all chord elements in the DOM
        for (var i = 1; i <= NUMBER_OF_CHORDS; i++) {
            audioChords.push(document.getElementById('chord' + i));
        }

        // Setup the playback channels
        for (var i = 0; i <= NUMBER_OF_CHANNELS; i++) {
            //audioChannels.push( new Audio('') );
        }

        context = canvas.getContext('2d');

        // Mouse events
        document.addEventListener('mousemove', documentMouseMoveHandler, false);
        document.addEventListener('mousedown', documentMouseDownHandler, false);
        document.addEventListener('mouseup', documentMouseUpHandler, false);
        canvas.addEventListener('dblclick', documentDoubleClickHandler, false);

        // Touch events
        document.addEventListener('touchstart', documentTouchStartHandler, false);
        document.addEventListener('touchmove', documentTouchMoveHandler, false);
        document.addEventListener('touchend', documentTouchEndHandler, false);

        // Keyboard events
        document.addEventListener('keydown', documentKeyDownHandler, false);

        // UI events
        document.getElementById('increaseSpeed').addEventListener('click', increaseSpeedClickHandler, false);
        document.getElementById('decreaseSpeed').addEventListener('click', decreaseSpeedClickHandler, false);
        document.getElementById('reset').addEventListener('click', resetClickHandler, false);
        document.getElementById('randomize').addEventListener('click', randomizeClickHandler, false);

        // Other events
        window.addEventListener('resize', windowResizeHandler, false);

        playhead = new Playhead();

        // Update the speed with a zero offset, this will enforce the max/min limits
        updateSpeed(0);

        // Force a window resize to position all elements
        windowResizeHandler();

        // Try to create keys from a possible deep link
        createKeysFromHash();

        // If there are no keys, show a intro to explain the usage
        if (keys.length == 0) {
            intro.style.display = 'block';
        }

        setInterval(loop, 1000 / 40);
    }
}

var NUMBER_OF_CHORDS, NUMBER_OF_CHANNELS, audioChords; 