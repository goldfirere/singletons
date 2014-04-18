INCLUDES = ../src ../dist/build compile-and-dump
OUTPUT = ../dist/build/quickly
OPTS = -O0 -Wall -Werror -fno-warn-orphans -fno-warn-missing-signatures
EXTS = TemplateHaskell DataKinds PolyKinds ScopedTypeVariables FlexibleContexts \
       FlexibleInstances GADTs TypeFamilies RankNTypes UndecidableInstances \
       TypeOperators MultiParamTypeClasses CPP LambdaCase MagicHash UnboxedTuples

ifndef THREADS
THREADS = 1
endif

GHCVERSIONSTR := $(shell ghc --numeric-version)
ifeq "$(GHCVERSIONSTR)" "7.6.3"
OPTS += -fno-warn-name-shadowing
else
# Parallel build with 2 threads. Using more threads can actually make the test
# slower - see https://github.com/jstolarek/singletons/issues/35
OPTS += -j2
endif


RUNGHC = ghc $(INCLUDES:%=-i%) -outputdir $(OUTPUT) Quickly.hs $(EXTS:%=-X%) $(OPTS) $(MORE_OPTS)

all:
	-rm -rf $(OUTPUT)
	-mkdir $(OUTPUT)
	$(RUNGHC)

build:
	$(RUNGHC)

.PHONY: all build
