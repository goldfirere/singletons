version=0.9.3
source=Data/Singletons.hs Data/Singletons/*.hs

.PHONY: build clean configure install

configure: dist/setup-config

build: dist/setup-config dist/build/libHSsingletons-$(version).a

install: dist/setup-config dist/build/libHSsingletons-$(version).a
	cabal install

dist/setup-config: singletons.cabal
	cabal clean
	cabal configure

dist/build/libHSsingletons-$(version).a: $(source)
	cabal build

clean:
	cabal clean
	find Data -name "*.hi" | xargs rm -f
	find Data -name "*.o"  | xargs rm -f
