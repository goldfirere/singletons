version=0.9.3
source=Data/Singletons.hs Data/Singletons/*.hs
test-source=Test/*.hs

.PHONY: build clean configure install tests

configure: dist/setup-config

build: dist/setup-config dist/build/libHSsingletons-$(version).a

tests: dist/tests-enabled.dummy dist/build/compile/compile dist/test/singletons-0.9.3-compile.log

install: dist/setup-config dist/build/libHSsingletons-$(version).a
	cabal install

dist/setup-config: singletons.cabal
	cabal clean
	cabal configure

dist/tests-enabled.dummy: singletons.cabal
	cabal clean
	cabal configure --enable-tests
	touch dist/tests-enabled.dummy

dist/build/libHSsingletons-$(version).a: $(source)
	cabal build

dist/build/compile/compile: dist/tests-enabled.dummy $(source) $(test-source)
	cabal build

dist/test/singletons-0.9.3-compile.log: dist/tests-enabled.dummy dist/build/compile/compile
	cabal test

clean:
	cabal clean
	find Data -name "*.hi" | xargs rm -f
	find Data -name "*.o"  | xargs rm -f
	find Test -name "*.hi" | xargs rm -f
	find Test -name "*.o"  | xargs rm -f
