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

app.ports.receiveScrollEventsForCommand.subscribe(function(elemId) {
    const elem = document.getElementById(elemId);
    if (elem) {
        console.log("receiving scroll events for " + elemId)
        elem.addEventListener('scroll', rsNesHandleScrollEventHandler);
    } else {
        console.log("receiveScrollEventsFor error: No element with id '" + elemId + "' found");
    }
});

