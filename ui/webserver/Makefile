JULIAHOME = $(shell pwd)/../..
include $(JULIAHOME)/Make.inc

CPP = g++
LIBS = -lpthread

LIBJULIA_RELEASE = $(JULIAHOME)/libjulia-release.$(SHLIB_EXT)
LIBJULIA_DEBUG = $(JULIAHOME)/libjulia-debug.$(SHLIB_EXT)

WEBSERVER_SRCS = webserver.cpp server.cpp network.cpp tinythread.cpp
WEBSERVER_OBJS = webserver.o server.o network.o tinythread.o

%.o: %.c
	$(CC) $(CFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.c
	$(CC) $(CFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-debug julia-release: webserver

webserver: $(WEBSERVER_SRCS)
	$(CPP) $(CXXFLAGS) $(SHIPFLAGS) -I$(JULIAHOME)/src -I$(JULIAHOME)/src/support -c $(WEBSERVER_SRCS)
	$(CPP) -o $@ $(SHIPFLAGS) $(WEBSERVER_OBJS) $(LIBJULIA_RELEASE) $(LIBS)
	cp webserver $(JULIAHOME)/webserver

clean:
	rm -f *.o *.do
	rm -f webserver
	rm -f $(JULIAHOME)/webserver