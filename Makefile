ELM_MAKE ?= elm make
ELMFLAGS ?= --optimize
UGLIFYJS ?= uglifyjs

all: docs/index.html NikumaruFont.en.woff2 COPYING

src/Translations.elm: public/translations/en.json
	pnpx elm-i18next-gen --source public/translations/en.json --target src/ --type both --overwrite

src/Main.elm.d.ts: src/InteropDefinitions.elm
	pnpx elm-ts-interop --output src/Main.elm.d.ts

docs/index.html: fonts elm.json src/Translations.elm src/Main.elm.d.ts src/*.elm src/*/*.elm src/*/*/*.elm
	pnpx vite build --base ''

fonts: public/NikumaruFont.en.woff2 public/NikumaruFont.ja.woff2

public/NikumaruFont.en.woff2: scripts/ftsubset.sh public/translations/*.json src/07にくまるフォント.otf
	./scripts/ftsubset.sh src/07にくまるフォント.otf public/NikumaruFont public/translations

public/NikumaruFont.ja.woff2: public/NikumaruFont.en.woff2

# Build `index.html` first to make `elm` unpack the packages, including the `LICENSE` files.
COPYING: scripts/collect_licenses.sh src/COPYING.in elm.json docs/index.html
	cp src/COPYING.in COPYING
	./scripts/collect_licenses.sh >> COPYING

check: check-elm check-ts

check-elm:
	elm make --output=/dev/null src/Main.elm

check-ts: src/Main.elm.d.ts
	pnpx eslint src/index.ts
	tsc --noEmit

clean:
	rm -r docs || true
	rm public/NikumaruFont.en.woff2 || true
	rm public/NikumaruFont.ja.woff2 || true
