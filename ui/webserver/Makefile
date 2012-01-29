JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

LIBS = -lpthread

WEBSERVER_SRCS = webserver.cpp server.cpp network.cpp jsoncpp.cpp

%.o: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release julia-debug: %: %-webserver

julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $(JULIAHOME)/$@ $(SHIPFLAGS) $(WEBSERVER_SRCS) $(LIBS)

julia-debug-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $(JULIAHOME)$@ $(DEBUGFLAGS) $(WEBSERVER_SRCS) $(LIBS)

clean:
	rm -f *.o *.do
	rm -f $(JULIAHOME)/julia-*-webserver
