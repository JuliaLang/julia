#
# ${R_HOME}/src/nmath/standalone/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/nmath/standalone

include $(top_builddir)/Makeconf

SOURCES_NMATH = \
	mlutils.c \
	d1mach.c i1mach.c \
	fmax2.c fmin2.c fprec.c fround.c ftrunc.c \
	sign.c fsign.c \
	imax2.c imin2.c \
	chebyshev.c log1p.c expm1.c lgammacor.c gammalims.c stirlerr.c bd0.c \
	gamma.c lgamma.c gamma_cody.c beta.c lbeta.c polygamma.c \
	bessel_i.c bessel_j.c bessel_k.c bessel_y.c \
	choose.c \
	snorm.c sexp.c \
	dgamma.c pgamma.c qgamma.c rgamma.c \
	dbeta.c pbeta.c qbeta.c rbeta.c \
	dunif.c punif.c qunif.c runif.c \
	dnorm.c pnorm.c qnorm.c rnorm.c \
	dlnorm.c plnorm.c qlnorm.c rlnorm.c \
	df.c pf.c qf.c rf.c dnf.c \
	dt.c pt.c qt.c rt.c dnt.c \
	dchisq.c pchisq.c qchisq.c rchisq.c rnchisq.c \
	dbinom.c pbinom.c qbinom.c rbinom.c \
	rmultinom.c \
	dcauchy.c pcauchy.c qcauchy.c rcauchy.c \
	dexp.c pexp.c qexp.c rexp.c \
	dgeom.c pgeom.c qgeom.c rgeom.c \
	dhyper.c phyper.c qhyper.c rhyper.c \
	dnbinom.c pnbinom.c qnbinom.c rnbinom.c \
	dpois.c ppois.c qpois.c rpois.c \
	dweibull.c pweibull.c qweibull.c rweibull.c \
	dlogis.c plogis.c qlogis.c rlogis.c \
	dnchisq.c pnchisq.c qnchisq.c \
	dnbeta.c pnbeta.c qnbeta.c \
	pnf.c pnt.c qnf.c qnt.c \
	ptukey.c qtukey.c \
	toms708.c wilcox.c \
	signrank.c
SOURCES = $(SOURCES_NMATH) std_unif.c
DEPENDS = $(SOURCES:.c=.d)
OBJECTS = $(SOURCES:.c=.o)

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = Makefile.in test.c sunif.c libRmath.pc.in

Rexeclibdir_LIBRARIES = libRmath.a
libRmath_a_SOURCES = $(SOURCES)
libRmath_a_OBJECTS = $(OBJECTS)

## <FIXME>
## Change to 'libRmath.la' when using libtool for shlibs.
## Remove when using automake ...
libRmath_la = libRmath$(DYLIB_EXT)
## </FIXME>
Rexeclibdir_LTLIBRARIES = $(libRmath_la)
libRmath_la_SOURCES = $(SOURCES)
libRmath_la_OBJECTS = $(OBJECTS:.o=.lo)
libRmath_la_LDFLAGS =

CLEANFILES = Makedeps *.d *.o *.lo test $(SOURCES)
DISTCLEANFILES = Makefile $(Rexeclibdir_LIBRARIES) $(Rexeclibdir_LTLIBRARIES)

DEFS = -DHAVE_CONFIG_H -DMATHLIB_STANDALONE
ALL_CPPFLAGS = $(R_XTRA_CPPFLAGS) -I$(srcdir)/.. $(CPPFLAGS) $(DEFS)

.SUFFIXES:
.SUFFIXES: .c .d .o .lo

.c.lo:
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS_LO) -c $< -o $@


all: Makefile inc static shared

inc: Makefile
	(cd ../../include; $(MAKE) Rmath.h)

## can't run test, as that depends on OS-specific way to set library path.
## clean is useful, but forces a remake each time which MM did not want
check: all # clean

Copy:
	@$(ECHO) "Copying source files"
	@test -f std_unif.c || $(LN_S) $(srcdir)/sunif.c std_unif.c
	@for f in $(SOURCES_NMATH); do \
	  test -f $${f} || $(LN_S) $(srcdir)/../$${f} . ; \
	done

static: inc ../../include/Rconfig.h Copy Makedeps
	@$(MAKE) $(Rexeclibdir_LIBRARIES)

shared: inc ../../include/Rconfig.h Copy Makedeps
	@$(MAKE) $(Rexeclibdir_LTLIBRARIES)

../../include/Rconfig.h:
	(cd $(@D); $(MAKE) $(@F))

Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	@cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

Makedeps: Makefile Copy ../../include/Rconfig.h
	@$(MAKE) $(DEPENDS)
	@cat $(DEPENDS) | $(SED) -e 's/^\([^:]*\)\.o\([ 	]\)*:/\1.o \1.lo\2:/' >> Makefile
	@touch $@

libRmath.a: $(libRmath_a_OBJECTS)
	$(AR) cr $@ $(libRmath_a_OBJECTS)
	$(RANLIB) $@

$(libRmath_la): $(libRmath_la_OBJECTS)
	$(DYLIB_LINK) -o $@ $(libRmath_la_LDFLAGS) $(libRmath_la_OBJECTS)

test: $(srcdir)/test.c
	$(CC) -o $@ $(ALL_CPPFLAGS) $(ALL_CFLAGS) $(srcdir)/test.c \
	  -L. -lRmath $(LIBM)

install: installdirs install-header # install-pc
#	@!(test -f $(libRmath_la)) || $(SHELL) $(top_srcdir)/tools/copy-if-change $(libRmath_la) $(DESTDIR)$(libdir)/$(libRmath_la)
#	@!(test -f libRmath.a) || $(SHELL) $(top_srcdir)/tools/copy-if-change libRmath.a $(DESTDIR)$(libdir)/libRmath.a
	@!(test -f $(libRmath_la)) || cp $(libRmath_la) $(R_FRAMEWORK_DIR)/Versions/$(FW_VERSION)/Resources/lib/$(libRmath_la) && install_name_tool -id $(R_FRAMEWORK_DIR)/Versions/$(FW_VERSION)/Resources/lib/$(libRmath_la) $(R_FRAMEWORK_DIR)/Versions/$(FW_VERSION)/Resources/lib/$(libRmath_la)
	@!(test -f libRmath.a) || $(SHELL) $(top_srcdir)/tools/copy-if-change libRmath.a $(R_FRAMEWORK_DIR)/Versions/$(FW_VERSION)/Resources/lib/libRmath.a

install-strip: installdirs install-header install-pc
	@if test -f $(libRmath_la); then \
	  $(INSTALL) $(libRmath_la) "$(libdir)/$(libRmath_la)"; \
	  if test -n "$(STRIP_LIBS)"; then \
	    $(STRIP_LIBS) "$(DESTDIR)$(libdir)/$(libRmath_la)"; \
	  fi; \
	fi
	@!(test -f libRmath.a) || $(SHELL) $(top_srcdir)/tools/copy-if-change libRmath.a $(DESTDIR)$(libdir)/libRmath.a

install-header:
	@(cd ../../include; $(MAKE) Rmath.h)
#	@$(SHELL) $(top_srcdir)/tools/copy-if-change ../../include/Rmath.h $(DESTDIR)$(includedir)/Rmath.h
	@$(SHELL) $(top_srcdir)/tools/copy-if-change ../../include/Rmath.h $(R_FRAMEWORK_DIR)/Versions/$(FW_VERSION)/Resources/include/Rmath.h

installdirs:
#	@$(MKINSTALLDIRS) $(DESTDIR)$(includedir)
#	@$(MKINSTALLDIRS) $(DESTDIR)$(libdir)
	@$(MKINSTALLDIRS) $(R_FRAMEWORK_DIR)/Versions/$(FW_VERSION)/Resources/include
	@$(MKINSTALLDIRS) $(R_FRAMEWORK_DIR)/Versions/$(FW_VERSION)/Resources/lib

## ?= is not portable make syntax, so we conditionalize this.
RMATH_FRAMEWORK_DIR ?= /Library/Frameworks/Rmath.framework
install-Rmath-framework: $(libRmath_la)
	@(cd ../../include; $(MAKE) Rmath.h)
	@$(MKINSTALLDIRS) $(RMATH_FRAMEWORK_DIR)/Versions/$(VERSION)/Headers
	@cp $(libRmath_la) $(RMATH_FRAMEWORK_DIR)/Versions/$(VERSION)/Rmath
	@install_name_tool -id $(RMATH_FRAMEWORK_DIR)/Versions/$(VERSION)/Rmath $(RMATH_FRAMEWORK_DIR)/Versions/$(VERSION)/Rmath
	@$(SHELL) $(top_srcdir)/tools/copy-if-change ../../include/Rmath.h $(RMATH_FRAMEWORK_DIR)/Versions/$(VERSION)/Headers/Rmath.h
	@rm -f $(RMATH_FRAMEWORK_DIR)/Versions/Current $(RMATH_FRAMEWORK_DIR)/Headers $(RMATH_FRAMEWORK_DIR)/Rmath
	@ln -sf $(VERSION) $(RMATH_FRAMEWORK_DIR)/Versions/Current
	@ln -sf Versions/Current/Headers $(RMATH_FRAMEWORK_DIR)/Headers
	@ln -sf Versions/Current/Rmath $(RMATH_FRAMEWORK_DIR)/Rmath

install-pc:
	@$(MKINSTALLDIRS) "$(DESTDIR)$(libdir)/pkgconfig"
	@$(SED) -e "s:@libdir:$(libdir):" -e "s:@includedir:$(includedir):" \
	  -e 's/@VERSION/$(VERSION)/' -e 's/@libm/$(LIBM)/' $(srcdir)/libRmath.pc.in \
	  > "$(DESTDIR)$(libdir)/pkgconfig/libRmath.pc"

uninstall:
	@rm -f $(DESTDIR)$(includedir)/Rmath.h $(DESTDIR)$(libdir)/$(libRmath_la) $(DESTDIR)$(libdir)/libRmath.a "$(DESTDIR)$(libdir)/pkgconfig/libRmath.pc"
	@rm -f $(R_FRAMEWORK_DIR)/Versions/$(VERSION)/Resources/include/Rmath.h $(R_FRAMEWORK_DIR)/Versions/$(VERSION)/Resources/lib/libRmath.a $(R_FRAMEWORK_DIR)/Versions/$(VERSION)/Resources/lib/$(libRmath_la)

mostlyclean: clean
clean:
	@-test -z "$(CLEANFILES)" || rm -f $(CLEANFILES)
	@-rm -rf .libs _libs
distclean: clean
	@-test -z "$(DISTCLEANFILES)" || rm -f $(DISTCLEANFILES)
maintainer-clean: distclean

distdir: $(DISTFILES)
	@for f in $(DISTFILES); do \
	  test -f $(distdir)/$${f} \
	    || ln $(srcdir)/$${f} $(distdir)/$${f} 2>/dev/null \
	    || cp -p $(srcdir)/$${f} $(distdir)/$${f}; \
	done

## Automagically generated dependencies:

mlutils.o mlutils.lo: mlutils.c ../../../src/include/config.h ../nmath.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
d1mach.o d1mach.lo: d1mach.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
i1mach.o i1mach.lo: i1mach.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
fmax2.o fmax2.lo: fmax2.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
fmin2.o fmin2.lo: fmin2.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
fprec.o fprec.lo: fprec.c ../../../src/include/config.h ../nmath.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
fround.o fround.lo: fround.c ../../../src/include/config.h ../nmath.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
ftrunc.o ftrunc.lo: ftrunc.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
sign.o sign.lo: sign.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
fsign.o fsign.lo: fsign.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
imax2.o imax2.lo: imax2.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
imin2.o imin2.lo: imin2.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
chebyshev.o chebyshev.lo: chebyshev.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
log1p.o log1p.lo: log1p.c ../../../src/include/config.h ../nmath.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
expm1.o expm1.lo: expm1.c ../../../src/include/config.h ../nmath.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
lgammacor.o lgammacor.lo: lgammacor.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
gammalims.o gammalims.lo: gammalims.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
stirlerr.o stirlerr.lo: stirlerr.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
bd0.o bd0.lo: bd0.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
gamma.o gamma.lo: gamma.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
lgamma.o lgamma.lo: lgamma.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
gamma_cody.o gamma_cody.lo: gamma_cody.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
beta.o beta.lo: beta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
lbeta.o lbeta.lo: lbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
polygamma.o polygamma.lo: polygamma.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
bessel_i.o bessel_i.lo: bessel_i.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../bessel.h
bessel_j.o bessel_j.lo: bessel_j.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../bessel.h
bessel_k.o bessel_k.lo: bessel_k.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../bessel.h
bessel_y.o bessel_y.lo: bessel_y.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../bessel.h
choose.o choose.lo: choose.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
snorm.o snorm.lo: snorm.c ../../../src/include/R_ext/Random.h \
  ../../../src/include/R_ext/Boolean.h ../nmath.h \
  ../../../src/include/config.h ../../../src/include/Rconfig.h \
  ../../../src/include/Rmath.h ../../../src/include/R_ext/RS.h \
  ../../../src/include/R_ext/Error.h ../../../src/include/R_ext/Print.h
sexp.o sexp.lo: sexp.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dgamma.o dgamma.lo: dgamma.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pgamma.o pgamma.lo: pgamma.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qgamma.o qgamma.lo: qgamma.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rgamma.o rgamma.lo: rgamma.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dbeta.o dbeta.lo: dbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pbeta.o pbeta.lo: pbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qbeta.o qbeta.lo: qbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rbeta.o rbeta.lo: rbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dunif.o dunif.lo: dunif.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
punif.o punif.lo: punif.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qunif.o qunif.lo: qunif.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
runif.o runif.lo: runif.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dnorm.o dnorm.lo: dnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pnorm.o pnorm.lo: pnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qnorm.o qnorm.lo: qnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rnorm.o rnorm.lo: rnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dlnorm.o dlnorm.lo: dlnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
plnorm.o plnorm.lo: plnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qlnorm.o qlnorm.lo: qlnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rlnorm.o rlnorm.lo: rlnorm.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
df.o df.lo: df.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pf.o pf.lo: pf.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qf.o qf.lo: qf.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rf.o rf.lo: rf.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dnf.o dnf.lo: dnf.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
dt.o dt.lo: dt.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pt.o pt.lo: pt.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qt.o qt.lo: qt.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rt.o rt.lo: rt.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dnt.o dnt.lo: dnt.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
dchisq.o dchisq.lo: dchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pchisq.o pchisq.lo: pchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qchisq.o qchisq.lo: qchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rchisq.o rchisq.lo: rchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
rnchisq.o rnchisq.lo: rnchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dbinom.o dbinom.lo: dbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pbinom.o pbinom.lo: pbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qbinom.o qbinom.lo: qbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rbinom.o rbinom.lo: rbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
rmultinom.o rmultinom.lo: rmultinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dcauchy.o dcauchy.lo: dcauchy.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pcauchy.o pcauchy.lo: pcauchy.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qcauchy.o qcauchy.lo: qcauchy.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rcauchy.o rcauchy.lo: rcauchy.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dexp.o dexp.lo: dexp.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pexp.o pexp.lo: pexp.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qexp.o qexp.lo: qexp.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rexp.o rexp.lo: rexp.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dgeom.o dgeom.lo: dgeom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pgeom.o pgeom.lo: pgeom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qgeom.o qgeom.lo: qgeom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rgeom.o rgeom.lo: rgeom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dhyper.o dhyper.lo: dhyper.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
phyper.o phyper.lo: phyper.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qhyper.o qhyper.lo: qhyper.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rhyper.o rhyper.lo: rhyper.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dnbinom.o dnbinom.lo: dnbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pnbinom.o pnbinom.lo: pnbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qnbinom.o qnbinom.lo: qnbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rnbinom.o rnbinom.lo: rnbinom.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dpois.o dpois.lo: dpois.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
ppois.o ppois.lo: ppois.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qpois.o qpois.lo: qpois.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rpois.o rpois.lo: rpois.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dweibull.o dweibull.lo: dweibull.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pweibull.o pweibull.lo: pweibull.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qweibull.o qweibull.lo: qweibull.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rweibull.o rweibull.lo: rweibull.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dlogis.o dlogis.lo: dlogis.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
plogis.o plogis.lo: plogis.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qlogis.o qlogis.lo: qlogis.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
rlogis.o rlogis.lo: rlogis.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h
dnchisq.o dnchisq.lo: dnchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pnchisq.o pnchisq.lo: pnchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qnchisq.o qnchisq.lo: qnchisq.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
dnbeta.o dnbeta.lo: dnbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pnbeta.o pnbeta.lo: pnbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qnbeta.o qnbeta.lo: qnbeta.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pnf.o pnf.lo: pnf.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
pnt.o pnt.lo: pnt.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qnf.o qnf.lo: qnf.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qnt.o qnt.lo: qnt.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
ptukey.o ptukey.lo: ptukey.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
qtukey.o qtukey.lo: qtukey.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
toms708.o toms708.lo: toms708.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
wilcox.o wilcox.lo: wilcox.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
signrank.o signrank.lo: signrank.c ../nmath.h ../../../src/include/config.h \
  ../../../src/include/Rconfig.h ../../../src/include/Rmath.h \
  ../../../src/include/R_ext/RS.h ../../../src/include/R_ext/Error.h \
  ../../../src/include/R_ext/Print.h ../dpq.h
std_unif.o std_unif.lo: std_unif.c
