JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(build_includedir) $(CFLAGS)

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
JLDFLAGS += $(LDFLAGS) $(NO_WHOLE_ARCHIVE) $(call exec,$(LLVM_CONFIG) --ldflags) $(OSLIBS) $(RPATH)

ifeq ($(USE_SYSTEM_LIBM),0)
ifneq ($(UNTRUSTED_SYSTEM_LIBM),0)
JLDFLAGS += $(WHOLE_ARCHIVE) $(build_libdir)/libopenlibm.a $(NO_WHOLE_ARCHIVE)
endif
endif

embedding-release: embedding

release debug:
	$(MAKE) embedding-$@

%.o: %.c
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@)
%.do: %.c
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)

embedding: $(build_bindir)/embedding$(EXE)
embedding-debug: $(build_bindir)/embedding-debug$(EXE)

$(build_bindir)/embedding$(EXE): embedding.o
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ -L$(build_private_libdir) -L$(build_shlibdir) -ljulia $(JLDFLAGS))
$(build_bindir)/embedding-debug$(EXE): embedding.do
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(build_private_libdir) -L$(build_shlibdir) -ljulia-debug $(JLDFLAGS))


clean: | $(CLEAN_TARGETS)
	rm -f *.o *.do
	rm -f $(build_bindir)/embedding-debug $(build_bindir)/embedding

.PHONY: clean release debug

