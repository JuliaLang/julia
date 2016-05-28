## Rmath-julia ##

RMATH_JULIA_OBJ_TARGET := $(build_shlibdir)/libRmath-julia.$(SHLIB_EXT)
RMATH_JULIA_OBJ_SOURCE := $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/src/libRmath-julia.$(SHLIB_EXT)

ifeq ($(USE_SYSTEM_DSFMT),0)
$(RMATH_JULIA_OBJ_SOURCE): $(DSFMT_OBJ_TARGET)
endif

RMATH_JULIA_FLAGS += CC="$(CC)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) \
			   OS="$(OS)" ARCH="$(ARCH)" \
			   USE_DSFMT=1 DSFMT_libdir="$(build_shlibdir)" \
			   DSFMT_includedir="$(build_includedir)"

$(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://api.github.com/repos/JuliaLang/Rmath-julia/tarball/v$(RMATH_JULIA_VER)
$(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/Makefile: $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz | $(SRCDIR)/srccache
	$(JLCHECKSUM) $<
	mkdir -p $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)
	$(TAR) -C $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER) --strip-components 1 -xf $<
	touch -c $@
$(RMATH_JULIA_OBJ_SOURCE): $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/Makefile
	$(MAKE) -C $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/src $(RMATH_JULIA_FLAGS) $(MAKE_COMMON)
	touch -c $@
$(RMATH_JULIA_OBJ_TARGET): $(RMATH_JULIA_OBJ_SOURCE) | $(build_shlibdir)
	cp $< $@
	$(INSTALL_NAME_CMD)libRmath-julia.$(SHLIB_EXT) $@

clean-Rmath-julia:
	-$(MAKE) -C $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER) clean
distclean-Rmath-julia: clean-Rmath-julia
	-rm -rf $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER) $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)

get-Rmath-julia: $(SRCDIR)/srccache/Rmath-julia-$(RMATH_JULIA_VER).tar.gz
configure-Rmath-julia: $(BUILDDIR)/Rmath-julia-$(RMATH_JULIA_VER)/Makefile
compile-Rmath-julia: $(RMATH_JULIA_OBJ_SOURCE)
check-Rmath-julia: compile-Rmath-julia
install-Rmath-julia: $(RMATH_JULIA_OBJ_TARGET)
