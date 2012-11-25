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

ifeq ($(OS),Darwin)
LIBS += -framework CoreServices
endif

WEBSERVER_SRCS = webserver.cpp server.cpp jsoncpp.cpp

%.o: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

	
ifeq ($(OS),WINNT)
LAUNCH_SCRIPT = launch-julia-webserver.bat
$(USRBIN)/$(LAUNCH_SCRIPT): ${JULIAHOME}/contrib/windows/*.bat
	cp ${JULIAHOME}/contrib/windows/*.bat ${USRBIN}
else
LAUNCH_SCRIPT = launch-julia-webserver
$(USRBIN)/$(LAUNCH_SCRIPT): $(LAUNCH_SCRIPT)
	cp $< $@
endif 
	
julia-release julia-debug: %: $(USRBIN)/%-webserver $(USRBIN)/$(LAUNCH_SCRIPT)

release debug:
	$(MAKE) julia-$@

$(USRBIN)/julia-release-webserver: $(WEBSERVER_SRCS) server.h
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(USRBIN)/julia-debug-webserver: $(WEBSERVER_SRCS) server.h
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(WEBSERVER_SRCS) $(LIBS)
	
clean:
	rm -f *.o *.do
	rm -f $(USRBIN)/julia-*-webserver
	rm -f $(USRBIN)/*.bat
