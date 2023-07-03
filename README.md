# todo-app-elm

## Tailwind CSS

### Install & run

``` sh
$ npm install --global tailwindcss
$ tailwindcss -i ./styles/input.css -o ./dist/output.css --watch 
```

## elm-live

### Run

```sh
$ elm-live src/Main.elm --port=1234 --hot --dir=./dist --start-page=index.html -- --output=./dist/main.js --debug
```
