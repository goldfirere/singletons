version=2.8
source=src/Data/Singletons.hs src/Data/Singletons/*.hs src/Data/Singletons/Prelude/* src/Data/Singletons/Promote/* src/Data/Singletons/Single/* src/Data/Promotion/* src/Data/Promotion/Prelude/*
test-suite=tests/SingletonsTestSuite.hs tests/SingletonsTestSuiteUtils.hs
test-byhand=tests/ByHand.hs tests/ByHandAux.hs

.PHONY: build byhand clean clean-tests configure install tests

configure: dist/setup-config

build: dist/setup-config dist/build/libHSsingletons-$(version).a

tests: dist/tests-enabled.dummy dist/build/compile/compile dist/test/singletons-$(version)-compile.log

install: dist/setup-config dist/build/libHSsingletons-$(version).a
	cabal install

byhand: tests/ByHand.o

tests/ByHand.o: $(test-byhand)
	ghc -itests tests/ByHand.hs

dist/setup-config: singletons.cabal
	cabal clean
	cabal configure

dist/tests-enabled.dummy: singletons.cabal
	cabal clean
	cabal configure --enable-tests
	touch dist/tests-enabled.dummy

dist/build/libHSsingletons-$(version).a: $(source)
	cabal build

dist/build/compile/compile: dist/tests-enabled.dummy $(source) $(test-suite)
	cabal build

dist/test/singletons-$(version)-compile.log: dist/tests-enabled.dummy dist/build/compile/compile
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
	rm -f tests/compile-and-dump/GradingClient/Main
