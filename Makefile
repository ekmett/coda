VERSION = $(shell cat .version)
HS_VERSION = 0.$(VERSION)
PLUGIN = coda-$(ARCH)-$(VERSION)
VSCE = node_modules/vsce/out/vsce
CODA = bin/coda
AUTHOR = ekmett

all:
	cabal build

install:
	cabal install

clean:
	cabal clean

dirty: bin/extension.js bin/coda
	mkdir -p ~/.vscode/extensions
	rm -rf ~/.vscode/extensions/$(AUTHOR).coda-$(VERSION)
	ln -s . ~/.vscode/extensions/$(AUTHOR).coda-$(VERSION)

reallyclean: clean
	rm -rf node_modules

distclean: reallyclean
	rm -rf package-lock.json

.PHONY: all clean dirty distclean reallyclean
