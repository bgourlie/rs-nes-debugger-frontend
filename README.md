## rs-nes-debugger-frontend

This is the front-end for the rs-nes debugger.

### Running the project

#### Prerequisites

The debugger currently relies on a [modified version](https://github.com/bgourlie/websocket/tree/websocket2-proposal) of
the websocket package that adds subscriptions for `onOpen` and `onClose`. Since Elm doesn't currently support adding
dependencies from a git repository, we use a third party tool package management tool:

    npm install elm-github-install -g

#### Running

    git clone https://github.com/bgourlie/rs-nes-debugger-frontend.git
    cd rs-nes-debugger-frontend
    npm install
    elm-install
    npm run dev
    
Then navigate your browser to `http://localhost:3001`.

