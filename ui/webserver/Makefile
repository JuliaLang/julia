JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

LIBS = -lpthread

WEBSERVER_SRCS = webserver.cpp server.cpp network.cpp jsoncpp.cpp

%.o: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release julia-debug: %: %-webserver

release debug:
	$(MAKE) julia-$@

julia-release-webserver: $(JULIAHOME)/julia-release-webserver
julia-debug-webserver: $(JULIAHOME)/julia-debug-webserver

$(JULIAHOME)/julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(JULIAHOME)/julia-debug-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(WEBSERVER_SRCS) $(LIBS)

clean:
	rm -f *.o *.do
	rm -f $(JULIAHOME)/julia-*-webserver
