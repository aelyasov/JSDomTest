function init() {

            //set up the fpsometer
            fpsMeter = new FpsMeter(0, "fish");
            fpsMeter.SetSettingsHtml("<div class='settingsLabel'>Choose number of fish</div><div class='control'><div class='control'><a class='control' href='javascript:createFish(1);'>1</a></div><div class='control'><a class='control' href='javascript:createFish(10);'>10</a></div><div class='control'><a class='control' href='javascript:createFish(20);'>20</a></div><div class='control'><a class='control' href='javascript:createFish(50);'>50</a></div><div class='control'><a class='control' href='javascript:createFish(100);'>100</a></div><div class='control'><a class='control' href='javascript:createFish(250);'>250</a></div><div class='control'><a class='control' href='javascript:createFish(500);'>500</a></div><div class='control'><a class='control' href='javascript:createFish(1000);'>1000</a></div>");

            //set up the canvas
            var tempCtx = document.createElement("canvas");
            tempCtx.id = "canvas1";
            tempCtx.setAttribute('width', WIDTH);
            tempCtx.setAttribute('height', HEIGHT);
            tempCtx.setAttribute('tabIndex', -1);
            document.body.insertBefore(tempCtx, document.body.firstChild);

            var tempCtx3 = document.createElement("canvas");
            tempCtx3.id = "background";
            tempCtx3.setAttribute('width', WIDTH);
            tempCtx3.setAttribute('height', HEIGHT);
            tempCtx3.setAttribute('tabIndex', -1);
            document.body.insertBefore(tempCtx3, document.body.firstChild);

            ctx = tempCtx.getContext("2d");

            ctx3 = tempCtx3.getContext("2d");

            //draw the background
            backgroundImage = document.getElementById('backgroundImage');
            drawBackground();

            //create the fish
            imageStrip = document.getElementById('imageStrip');
            createFish(startFish);

            //start animation
            setInterval(function() {
                draw();
            }, 16);

            window.addEventListener("resize", OnWindowResize, false);

        }