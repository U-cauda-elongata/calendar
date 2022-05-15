all: app.js COPYING

build/elm.js: src/*.elm src/*/*.elm src/*/*/*.elm
	elm make --optimize --output=build/elm.js src/Main.elm

app.js: elm.json build/elm.js customElements.js src/notice.js
	uglifyjs build/elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
	| uglifyjs --mangle --output build/elm.min.js \
	&& uglifyjs customElements.js --compress --mangle --output build/customElements.min.js \
	&& uglifyjs src/notice.js build/elm.min.js build/customElements.min.js --comments all --output app.js \
	|| cat src/notice.js build/elm.js customElements.js > app.js

COPYING: scripts/collect_licenses.sh COPYING.in elm.json
	cp COPYING.in COPYING
	./scripts/collect_licenses.sh >> COPYING

clean:
	rm build/elm.js || true
	rm build/elm.min.js || true
	rm build/customElements.min.js || true
	rm app.js || true
