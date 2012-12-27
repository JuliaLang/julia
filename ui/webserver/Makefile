JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

LIBS = -lpthread $(BUILD)/$(JL_LIBDIR)/libuv.a 
ifeq ($(OS), Linux)
LIBS += -lrt 
endif

ifeq ($(OS),WINNT)
LIBS += -lWs2_32 -lIphlpapi -lpsapi
endif

ifeq ($(OS),Darwin)
LIBS += -framework ApplicationServices
endif

WEBSERVER_SRCS = webserver.o server.o jsoncpp.o

%.o: %.cpp
	$(QUIET_CC)$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.cpp
	$(QUIET_CC)$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) -c $< -o $@

LAUNCH_SCRIPT = launch-julia-webserver
ifeq ($(OS),WINNT)
LAUNCH_SCRIPT = launch-julia-webserver.bat
endif 

julia-release julia-debug: %: $(BUILD)/bin/%-webserver $(BUILD)/bin/$(LAUNCH_SCRIPT)

release debug:
	$(MAKE) julia-$@

$(BUILD)/bin/julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(LDFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(BUILD)/bin/julia-debug-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(LDFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(BUILD)/bin/$(LAUNCH_SCRIPT): $(LAUNCH_SCRIPT)
	cp $< $@

clean:
	rm -f *.o *.do
	rm -f $(BUILD)/bin/julia-*-webserver
