INCLUDES = ../src ../dist/build compile-and-dump
OUTPUT = ../dist/build/quickly
OPTS = -Wall -Werror -fno-warn-orphans -fno-warn-missing-signatures
EXTS = TemplateHaskell DataKinds PolyKinds ScopedTypeVariables FlexibleContexts \
       FlexibleInstances GADTs TypeFamilies RankNTypes UndecidableInstances \
       TypeOperators MultiParamTypeClasses

GHCVERSIONSTR := $(shell ghc --numeric-version)
ifneq "$(GHCVERSIONSTR)" "7.6.3"
EXTS += EmptyCase
endif

RUNGHC = ghc $(INCLUDES:%=-i%) -outputdir $(OUTPUT) Quickly.hs $(EXTS:%=-X%) $(OPTS)

all:
	-rm -rf $(OUTPUT)
	-mkdir $(OUTPUT)
	$(RUNGHC)

build:
	$(RUNGHC)

.PHONY: all build
