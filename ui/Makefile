JULIAHOME = ..
include ../Make.inc

LLTDIR = ../src/support
FLISPDIR = ../src/flisp
ifeq ($(USE_SYSTEM_READLINE), 1)
READLINE = -lreadline -lncurses -lcurses
else
READLINE = $(EXTROOT)/lib/libreadline.a -lncurses -lcurses
endif

FLAGS = -Wall -Wno-strict-aliasing -fno-omit-frame-pointer \
	    -I../src -I$(LLTDIR) -I$(EXTROOT)/include $(CFLAGS)

DEBUGFLAGS = $(FLAGS)
SHIPFLAGS = $(FLAGS)

#ifeq ($(USEGCC),1)
#DEBUGFLAGS += -static-libgcc -static-libstdc++
#SHIPFLAGS += -static-libgcc -static-libstdc++
#endif

julia-debug julia-release: %: %-basic %-readline

debug release:
	$(MAKE) julia-$@

%.o: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c repl.h
	$(QUIET_CC) $(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release-basic: ../julia-release-basic
julia-debug-basic: ../julia-debug-basic
julia-release-readline: ../julia-release-readline
julia-debug-readline: ../julia-debug-readline

../julia-release-basic: repl.o repl-basic.o ../src/libjulia-release.a
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $^ -o $@ ../src/libjulia-release.a $(LIBS)
../julia-debug-basic: repl.do repl-basic.do ../src/libjulia-debug.a
	$(QUIET_LINK) $(CXX) $(DEBUGFLAGS) $^ -o $@ ../src/libjulia-debug.a $(LIBS)

../julia-release-readline: repl.o repl-readline.o ../src/libjulia-release.a
	$(QUIET_LINK) $(CXX) $(SHIPFLAGS) $^ -o $@ ../src/libjulia-release.a $(READLINE) $(LIBS)
../julia-debug-readline: repl.do repl-readline.do ../src/libjulia-debug.a
	$(QUIET_LINK) $(CXX) $(DEBUGFLAGS) $^ -o $@ ../src/libjulia-debug.a $(READLINE) $(LIBS)

clean:
	rm -f *.o *.do
	rm -f ../julia-*-basic
	rm -f ../julia-*-readline
	rm -f ../julia-*-web
