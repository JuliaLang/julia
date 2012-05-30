JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

LIBS = -lpthread $(USRLIB)/uv.a 
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

julia-release julia-debug: %: $(USRBIN)/%-webserver $(USRBIN)/launch-julia-webserver

release debug:
	$(MAKE) julia-$@

$(USRBIN)/julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(USRBIN)/julia-debug-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(USRBIN)/launch-julia-webserver: launch-julia-webserver
	cp launch-julia-webserver $@
	cp -r ../website $(USR)/lib/julia

clean:
	rm -f *.o *.do
	rm -f $(USRBIN)/julia-*-webserver
