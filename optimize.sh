#!/bin/sh

set -e

js="./dist/main.js"
min="./dist/main.min.js"

elm make --optimize --output=$js "$@"

uglifyjs $js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $min

echo "Compiled size: $(stat -f%z $js) bytes ($js)"
echo "Minified size: $(stat -f%z $min) bytes ($min)"
echo "Gzipped size: $(gzip $min -c | stat -f%z) bytes"
