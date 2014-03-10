INCLUDES = ../src ../dist/build compile-and-dump
OUTPUT = ../dist/build/quickly
EXTS = TemplateHaskell DataKinds PolyKinds ScopedTypeVariables FlexibleContexts \
       FlexibleInstances GADTs TypeFamilies RankNTypes UndecidableInstances \
       TypeOperators MultiParamTypeClasses

GHCVERSIONSTR := $(shell ghc --numeric-version)
ifneq "$(GHCVERSIONSTR)" "7.6.3"
EXTS += EmptyCase
endif

all:
	-rm -rf $(OUTPUT)
	-mkdir $(OUTPUT)
	ghc $(INCLUDES:%=-i%) -outputdir $(OUTPUT) Quickly.hs $(EXTS:%=-X%)
