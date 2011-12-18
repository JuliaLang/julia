JULIAHOME = $(shell pwd)/../..
include $(JULIAHOME)/Make.inc

LIBS = -lpthread

WEBSERVER_SRCS = webserver.cpp server.cpp network.cpp jsoncpp.cpp

%.o: %.c
	$(QUIET_CC)$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-debug julia-release: %: %-webserver

julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_CC)$(CXX) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(WEBSERVER_SRCS) $(LIBS)
	cp $@ $(JULIAHOME)/$@

julia-debug-webserver: $(WEBSERVER_SRCS)
	$(CXX) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(WEBSERVER_SRCS) $(LIBS)
	cp $@ $(JULIAHOME)/$@

clean:
	rm -f *.o *.do
	rm -f julia-release-webserver
	rm -f julia-debug-webserver
	rm -f $(JULIAHOME)/julia-release-webserver
	rm -f $(JULIAHOME)/julia-debug-webserver
