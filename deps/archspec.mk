## ARCHSPEC_CPP ##
# archspec_cpp is included as a git submodule in deps/archspec_cpp

ARCHSPEC_CPP_DIR := $(JULIAHOME)/deps/archspec_cpp

# Include paths for archspec headers (archspec + nlohmann/json)
ARCHSPEC_INC := $(ARCHSPEC_CPP_DIR)/include $(ARCHSPEC_CPP_DIR)/extern/json/single_include

# Static library (built in submodule's build directory)
ARCHSPEC_LIB := $(ARCHSPEC_CPP_DIR)/build/lib/libarchspec.a

# Build archspec_cpp
$(ARCHSPEC_LIB):
	$(MAKE) -C $(ARCHSPEC_CPP_DIR) $(ARCHSPEC_LIB)

.PHONY: compile-archspec clean-archspec

compile-archspec: $(ARCHSPEC_LIB)

clean-archspec:
	$(MAKE) -C $(ARCHSPEC_CPP_DIR) clean

