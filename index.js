'use strict';

require('./index.html');
require('./src/Stylesheets');
const Elm = require('./src/Main');

const app = Elm.Main.fullscreen();

app.ports.scrollElementIntoView.subscribe(function(id) {
    const elem = document.getElementById(id);
    if(elem) {
        elem.scrollIntoView();
    }
});
