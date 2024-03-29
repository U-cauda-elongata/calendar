ELM_MAKE ?= elm make
ELMFLAGS ?= --optimize
UGLIFYJS ?= uglifyjs

all: app.js NikumaruFont.en.woff2 COPYING

src/Translations.elm: translations/en.json
	npx elm-i18next-gen --source translations/en.json --target src/ --type both --overwrite

build/elm.js: src/Translations.elm src/*.elm src/*/*.elm src/*/*/*.elm
	$(ELM_MAKE) $(ELMFLAGS) --output=build/elm.js src/Main.elm

app.js: elm.json build/elm.js src/customElements.js src/feeds.js src/observances.js src/init.js src/notice.js
	$(UGLIFYJS) build/elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
	| $(UGLIFYJS) --mangle --output build/elm.min.js \
	&& $(UGLIFYJS) src/customElements.js --compress --mangle --toplevel --output build/customElements.min.js \
	&& $(UGLIFYJS) src/feeds.js src/observances.js src/init.js --compress --mangle --toplevel --output build/init.min.js \
	&& $(UGLIFYJS) src/notice.js build/elm.min.js build/customElements.min.js build/init.min.js --mangle --toplevel --comments all --output app.js \
	|| cat src/notice.js build/elm.js src/customElements.js src/feeds.js src/observances.js src/init.js > app.js

fonts: NikumaruFont.en.woff2 NikumaruFont.ja.woff2

NikumaruFont.en.woff2: scripts/ftsubset.sh translations/*.json src/07にくまるフォント.otf
	./scripts/ftsubset.sh src/07にくまるフォント.otf NikumaruFont translations

NikumaruFont.ja.woff2: NikumaruFont.en.woff2

COPYING: scripts/collect_licenses.sh src/COPYING.in elm.json
	cp src/COPYING.in COPYING
	./scripts/collect_licenses.sh >> COPYING

clean:
	rm build/elm.js || true
	rm build/elm.min.js || true
	rm build/customElements.min.js || true
	rmdir build/ || true
	rm app.js || true
	rm NikumaruFont.en.woff2 || true
	rm NikumaruFont.ja.woff2 || true
