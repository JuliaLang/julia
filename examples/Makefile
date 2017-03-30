SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILDDIR := .
JULIAHOME := $(abspath $(SRCDIR)/..)
include $(JULIAHOME)/Make.inc

embedding_binary := $(abspath $(build_libexecdir)/embedding$(JULIA_LIBSUFFIX)$(EXE))

release: embedding
debug: embedding-debug

embedding: $(embedding_binary)
embedding-debug: $(embedding_binary)

$(embedding_binary): $(wildcard embedding/*)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/examples/embedding $(JULIA_BUILD_MODE) \
                                JULIA="$(JULIA_EXECUTABLE)" BIN="$(build_libexecdir)" \
                                SPAWN="$(spawn)" CC="$(CC)"

clean:
	-rm -f $(embedding_binary) $(embedding_binary)-debug

.PHONY: all embedding clean

