# roisalen: elm-frontend

## How to build the application
Setup Elm: https://guide.elm-lang.org/install.html
```
$ elm make src/Main.elm --output=main.js
```
Then open `index.html` in a web browser.

## How to develop locally
The recommended way to develop this locally is by using elm live ([install instructions here](https://github.com/architectcodes/elm-live), which will rebuild the application and reload your browser when any file has changed. Execute `./dev.sh` to start elm live.
