all:
	npm install

clean:
	rm -rf dist dist-newstyle

reallyclean: clean
	rm -rf node_modules .cabal-sandbox cabal.sandbox.config

distclean: reallyclean
	rm -rf package-lock.json

.PHONY: all clean reallyclean distclean
