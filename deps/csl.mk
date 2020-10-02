ifneq ($(USE_BINARYBUILDER_CSL),0)
COMPILER_SUPPORT_LIBRARIES_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl/releases/download/CompilerSupportLibraries-v$(COMPILER_SUPPORT_LIBRARIES_VER)+$(COMPILER_SUPPORT_LIBRARIES_BB_REL)
COMPILER_SUPPORT_LIBRARIES_BB_NAME := CompilerSupportLibraries.v$(COMPILER_SUPPORT_LIBRARIES_VER)
$(eval $(call bb-install,csl,COMPILER_SUPPORT_LIBRARIES,true))
endif
