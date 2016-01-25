# Make sure that you are building openblas with OPENBLAS_DYNAMIC_ARCH=1
# You may have to wipe your openblas build to ensure that it is built
# with support for all architectures, or else performance may suffer.

JULIAHOME := $(abspath ../../..)
include ../../../Make.inc

# X.Y.Z or X.Y.Z-hash
DMG_VERSION_SUFFIX := $(shell [ $$(git describe --tags --exact-match 2>/dev/null) ] && echo $(JULIA_VERSION) || echo $(JULIA_VERSION)-$(JULIA_COMMIT))

# X.Y
APP_VERSION_SUFFIX := $(shell echo $(JULIA_VERSION) | grep -o '^[0-9]\+.[0-9]\+')

DMG_NAME := Julia-$(DMG_VERSION_SUFFIX).dmg
APP_NAME := Julia-$(APP_VERSION_SUFFIX).app
VOL_NAME := Julia-$(JULIA_VERSION)

all: clean $(DMG_NAME)

# reuse same virtualenv as docs
JULIA_DEPS := $(JULIAHOME)/deps
JULIA_ENV := $(JULIA_DEPS)/build/julia-env
ACTIVATE  := $(JULIA_ENV)/bin/activate
DMGBUILD_ENV := $(JULIA_ENV)/bin/dmgbuild
DMGBUILD := . $(ACTIVATE) && dmgbuild

$(ACTIVATE):
	$(MAKE) -C $(JULIA_DEPS) install-virtualenv
	touch -c $@

$(DMGBUILD_ENV): $(ACTIVATE)
	. $(ACTIVATE) && pip install dmgbuild
	touch -c $@

$(DMG_NAME): $(DMGBUILD_ENV) $(APP_NAME) dmg.py background.pdf julia.icns
	$(DMGBUILD) -s dmg.py -D app=$(APP_NAME) $(VOL_NAME) $(DMG_NAME)

$(APP_NAME): julia.icns
	$(MAKE) -C ../../../deps install-git
	$(MAKE) -C ../../.. binary-dist
	tar zxf ../../../julia-*.tar.gz
	mv julia-* julia
	-mkdir -p ./julia/libexec ./julia/share
	-cp -a $(build_libexecdir)/git* ./julia/libexec
	-cp -a $(build_datarootdir)/git* ./julia/share
	rm -f julia/lib/*.{a,la}
	platypus -a Julia -p /bin/bash -V $(JULIA_VERSION) -R -u "The Julia Project" -i julia.icns -Q julia.icns -o "None" -I org.julialang.julia -x -f julia script ./$(APP_NAME)
	-codesign -s "AFB379C0B4CBD9DB9A762797FC2AB5460A2B0DBE" --deep ./$(APP_NAME)

clean:
	rm -rf julia *.app *.dmg

.PHONY: clean all
