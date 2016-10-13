'use strict';

const d = require('6502-disasm');
require('./index.html');
require('./src/Stylesheets');
const Elm = require('./src/Main');

const app = Elm.Main.fullscreen();

app.ports.decode.subscribe(function(bytes) {
    const disasm = new d.Disassembler(bytes);
    const res = disasm.decode();
    app.ports.decoded.send(res);
});