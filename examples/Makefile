JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	-I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -I$(BUILD)/include $(CFLAGS) 

DEBUGFLAGS += $(FLAGS)
SHIPFLAGS += $(FLAGS)
JLDFLAGS += $(LDFLAGS) $(NO_WHOLE_ARCHIVE) $(call exec,$(LLVM_CONFIG) --ldflags) $(OSLIBS) $(RPATH)

ifeq ($(USE_SYSTEM_LIBM),0)
ifneq ($(UNTRUSTED_SYSTEM_LIBM),0)
ifeq ($(OS),WINNT)
JLDFLAGS += $(WHOLE_ARCHIVE) $(BUILD)/lib/libopenlibm.a $(NO_WHOLE_ARCHIVE)
else
JLDFLAGS += $(WHOLE_ARCHIVE) $(BUILD)/$(JL_LIBDIR)/libopenlibm.a $(NO_WHOLE_ARCHIVE)
endif
endif
endif

embedding-release: embedding

release debug:
	$(MAKE) embedding-$@

%.o: %.c
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@)
%.do: %.c
	@$(call PRINT_CC, $(CC) $(CPPFLAGS) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@)

embedding: $(BUILD)/bin/embedding$(EXE)
embedding-debug: $(BUILD)/bin/embedding-debug$(EXE)

$(BUILD)/bin/embedding$(EXE): embedding.o
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(SHIPFLAGS) $^ -o $@ -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) -ljulia $(JLDFLAGS))
$(BUILD)/bin/embedding-debug$(EXE): embedding.do
	@$(call PRINT_LINK, $(CXX) $(LINK_FLAGS) $(DEBUGFLAGS) $^ -o $@ -L$(BUILD)/$(JL_PRIVATE_LIBDIR) -L$(BUILD)/$(JL_LIBDIR) -ljulia-debug $(JLDFLAGS))


clean: | $(CLEAN_TARGETS)
	rm -f *.o *.do
	rm -f $(BUILD)/bin/embedding-debug $(BUILD)/bin/embedding

.PHONY: clean release debug

