## rs-nes-debugger-frontend

This is the front-end for the rs-nes debugger.

### Running the project

    git clone https://github.com/bgourlie/rs-nes-debugger-frontend.git
    cd rs-nes-debugger-frontend
    npm install
    elm-make
    npm run dev
    
Then navigate your browser to `http://localhost:3001`.

#### Unfortunate hacks

The debugger currently relies on a modified version of the websocket
package that adds subscriptions for `onOpen` and `onClose`. Using the
modified package involves cloning 
[this](https://github.com/bgourlie/websocket/tree/websocket2-proposal)
branch and symlinking it the appropriate location in the `elm-stuff` directory.

These changes have been proposed as version 2.0.0 of the websocket package which 
can be viewed [here](https://github.com/elm-lang/websocket/pull/19).