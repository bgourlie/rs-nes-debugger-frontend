'use strict';

require('./index.html');
require('./src/Stylesheets');
const Elm = require('./src/Main');

const app = Elm.Main.fullscreen();

app.ports.scrollElementIntoViewCommand.subscribe(function(cls) {
    const elem = document.getElementsByClassName(cls)[0];
    if (elem) {
        elem.scrollIntoView();
    }
});
