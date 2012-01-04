JULIAHOME = ../..
include ../../Make.inc

LIBS = -lpthread

WEBSERVER_SRCS = webserver.cpp server.cpp network.cpp jsoncpp.cpp

%.o: %.c
	$(QUIET_CC) $(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(QUIET_CC) $(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-debug julia-release: %: %-webserver

julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(WEBSERVER_SRCS) $(LIBS)
	cp $@ $(JULIAHOME)/$@

julia-debug-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(WEBSERVER_SRCS) $(LIBS)
	cp $@ $(JULIAHOME)/$@

clean:
	rm -f *.o *.do
	rm -f julia-release-webserver
	rm -f julia-debug-webserver
	rm -f ../../julia-release-webserver
	rm -f ../../julia-debug-webserver
