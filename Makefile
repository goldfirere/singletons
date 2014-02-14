version=0.9.3
source=src/Data/Singletons.hs src/Data/Singletons/*.hs
test-source=tests/SingletonsTestSuite.hs tests/SingletonsTestSuiteUtils.hs

.PHONY: build clean clean-tests configure install tests

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

clean: clean-tests
	cabal clean
	find src   -name "*.hi"     | xargs rm -f
	find src   -name "*.dyn_hi" | xargs rm -f
	find src   -name "*.o"      | xargs rm -f
	find src   -name "*.dyn_o"  | xargs rm -f

clean-tests:
	find tests -name "*.hi"     | xargs rm -f
	find tests -name "*.dyn_hi" | xargs rm -f
	find tests -name "*.o"      | xargs rm -f
	find tests -name "*.dyn_o"  | xargs rm -f
	find tests -name "*.actual" | xargs rm -f
	find tests -name "*.golden" | xargs rm -f
	rm -f tests/compile-and-dump/GradingClient/Main
