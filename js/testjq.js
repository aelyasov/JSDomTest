(function() {
    'use strict';

    var env = require('jsdom').env,
        html = '<html><body><h1>Hello World!</h1><p class="hello">Heya Big World!</body></html>';

    // first argument can be html string, filename, or url
    env(html, function(errors, window) {
        console.log(errors);
        var jquery = require('jquery');
        var $ = jquery(window);
        console.log(window.document);
        console.log($('.hello').text());
    });
}());
