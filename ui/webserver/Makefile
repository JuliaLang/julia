JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

LIBS = -lpthread $(USR)/$(JL_LIBDIR)/libuv.a 
ifeq ($(OS), Linux)
LIBS += -lrt 
endif

ifeq ($(OS),WINNT)
LIBS += -lWs2_32 -lIphlpapi -lpsapi
endif

WEBSERVER_SRCS = webserver.cpp server.cpp jsoncpp.cpp

%.o: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

	
LAUNCH_SCRIPT = launch-julia-webserver
ifeq ($(OS),WINNT)
LAUNCH_SCRIPT = launch-julia-webserver.bat
endif 
	
julia-release julia-debug: %: $(USRBIN)/%-webserver $(USRBIN)/$(LAUNCH_SCRIPT)

release debug:
	$(MAKE) julia-$@

$(USRBIN)/julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(USRBIN)/julia-debug-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(WEBSERVER_SRCS) $(LIBS)
	
$(USRBIN)/$(LAUNCH_SCRIPT): $(LAUNCH_SCRIPT)
	cp $< $@

clean:
	rm -f *.o *.do
	rm -f $(USRBIN)/julia-*-webserver
