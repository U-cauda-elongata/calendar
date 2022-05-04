all:
	elm make --optimize --output=elm.js src/Main.elm
	uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
	| uglifyjs --mangle --output elm.min.js \
	&& uglifyjs --compress --mangle --output customElements.min.js < customElements.js \
	&& uglifyjs elm.min.js customElements.min.js --output app.js \
	&& rm elm.min.js customElements.min.js \
	|| cat elm.js customElements.js > app.js \
	&& rm elm.js
