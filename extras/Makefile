JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

OS = $(shell uname)
ARCH = $(shell uname -m)
LIBS = glpk-wrapper

ifeq ($(USE_SYSTEM_GLPK), 0)
LIBS += glpk
endif

WGET = wget
WGET_DASH_O = wget -O
ifeq ($(OS), Darwin)
WGET = curl -kLO
WGET_DASH_O = curl -kLo
endif

ifeq ($(OS), FreeBSD)
WGET = curl -LO
WGET_DASH_O = curl -Lo
endif

GLPK_VER = 4.43
GLPK_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

default: install
compile: $(addprefix compile-, $(LIBS)) glpk_h.jl
install: $(addprefix install-, $(LIBS))
cleanall: $(addprefix clean-, $(LIBS)) clean-rest
distclean: $(addprefix distclean-, $(LIBS))

## GLPK ##

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPK_PREFIX = /usr/
GLPK_OBJ_TARGET =
else
GLPK_PREFIX = glpk-$(GLPK_VER)/
GLPK_OBJ_TARGET = $(EXTROOTLIB)/libglpk.$(SHLIB_EXT)
endif


compile-glpk: install-glpk
install-glpk: $(GLPK_OBJ_TARGET) $(EXTROOTLIB)/libglpk_wrapper.$(SHLIB_EXT) glpk_h.jl

glpk-$(GLPK_VER).tar.gz:
	$(WGET) ftp://ftp.gnu.org/gnu/glpk/$@

glpk-$(GLPK_VER)/configure: glpk-$(GLPK_VER).tar.gz
	tar zxf $<
	touch $@
glpk-$(GLPK_VER)/config.status: glpk-$(GLPK_VER)/configure
	cd glpk-$(GLPK_VER) && \
	./configure --prefix=$(abspath $(EXTROOT))
$(GLPK_OBJ_TARGET): glpk-$(GLPK_VER)/config.status
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C glpk-$(GLPK_VER) install
	touch $@

glpk_h.jl:
	$(QUIET_PERL) cpp -dM $(GLPK_PREFIX)/include/glpk.h | perl -nle '/^\s*#define\s+(GLP\w*)\s*\(?($(GLPK_CONST))\)?\s*$$/ and print "const $$1 = int32($$2)"' | sort > $@

clean-glpk:
	-$(MAKE) $(jPARALLEL_BUILD_JOBS) -C glpk-$(GLPK_VER) uninstall || true
	-$(MAKE) $(jPARALLEL_BUILD_JOBS) -C glpk-$(GLPK_VER) clean
	rm -f glpk_h.jl
distclean-glpk: clean-glpk
	rm -rf glpk-$(GLPK_VER).tar.bz2 glpk-$(GLPK_VER)


## GLPK Wrapper

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPKW_INC = -I /usr/include/
GLPKW_LIB = -lglpk
else
GLPKW_INC = -I glpk-$(GLPK_VER)/include/
GLPKW_LIB = -L$(EXTROOTLIB)/ -lglpk
endif


$(EXTROOTLIB)/libglpk_wrapper.$(SHLIB_EXT): glpk_wrapper.c $(GLPK_OBJ_TARGET)
	mkdir -p $(EXTROOTLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared -fPIC $(GLPKW_INC) glpk_wrapper.c $(GLPKW_LIB) -o $(EXTROOTLIB)/libglpk_wrapper.$(SHLIB_EXT) -Wl,-rpath,$(EXTROOTLIB)
	touch $@
install-glpk-wrapper: $(EXTROOTLIB)/libglpk_wrapper.$(SHLIB_EXT) glpk_wrapper.c

clean-glpk-wrapper:
	rm -f $(GLPK_OBJ_TARGET) $(EXTROOTLIB)/libglpk_wrapper.$(SHLIB_EXT)
distclean-glpk-wrapper: clean-glpk-wrapper


## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
