include ../Make.inc

default: all

.PHONY: libccalltest

all libccalltest: libccalltest.$(SHLIB_EXT)
libccalltest.$(SHLIB_EXT): ccalltest.c
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -fPIC -shared -o $@ $(LDFLAGS) -DCC="$(CC)")

ccalltest: ccalltest.c libccalltest.$(SHLIB_EXT)
	@$(call PRINT_CC, $(CC) $(CFLAGS) $(DEBUGFLAGS) -O3 $< -o $@ $(LDFLAGS) -DCC="$(CC)")
