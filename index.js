'use strict';

const d = require('6502-disasm');
require('./index.html');
require('./src/Stylesheets');
const Elm = require('./src/Main');

const app = Elm.Main.fullscreen();

app.ports.decode.subscribe(function(args) {
    const unpacked = new Uint8Array(Int32Array.from(args[0]).buffer);
    const disasm = new d.Disassembler(unpacked);
    const res = disasm.decode(args[1], args[2]);
    app.ports.decoded.send(res);
});