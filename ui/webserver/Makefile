JULIAHOME = $(abspath ../..)
include $(JULIAHOME)/Make.inc

override CFLAGS += $(JCFLAGS)
override CXXFLAGS += $(JCXXFLAGS)

LIBS = $(BUILD)/$(JL_LIBDIR)/libuv.a 
ifeq ($(OS), Linux)
LIBS += -lpthread -lrt 
endif

ifeq ($(OS),WINNT)
LIBS += -lws2_32 -liphlpapi -lpsapi
endif

ifeq ($(OS),Darwin)
LIBS += -framework ApplicationServices
endif

WEBSERVER_SRCS = webserver.o server.o jsoncpp.o

%.o: %.cpp *.h
	$(QUIET_CC)$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(SHIPFLAGS) -c $< -o $@
%.do: %.cpp *.h
	$(QUIET_CC)$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(DEBUGFLAGS) -c $< -o $@

julia-release julia-debug: %: $(BUILD)/bin/%-webserver $(BUILD)/bin/$(LAUNCH_SCRIPT) $(BUILD)/etc/nginx.conf

release debug:
	$(MAKE) julia-$@

$(BUILD)/bin/julia-release-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $(SHIPFLAGS) $(LDFLAGS) $(WEBSERVER_SRCS) $(LIBS)

$(BUILD)/bin/julia-debug-webserver: $(WEBSERVER_SRCS)
	$(QUIET_LINK) $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $(DEBUGFLAGS) $(LDFLAGS) $(WEBSERVER_SRCS) $(LIBS)

ifeq ($(OS),WINNT)
LAUNCH_SCRIPT = launch-julia-webserver.bat
else
LAUNCH_SCRIPT = launch-julia-webserver
endif
$(BUILD)/bin/$(LAUNCH_SCRIPT): $(LAUNCH_SCRIPT)
	cp $< $@

$(BUILD)/etc/nginx.conf: nginx*.conf
ifeq ($(OS),WINNT)
	cp nginx-win.conf $@
else
	cp nginx.conf $@
endif


clean:
	rm -f *.o *.do
	rm -f $(BUILD)/bin/julia-*-webserver
	rm -f $(BUILD)/bin/*.bat
