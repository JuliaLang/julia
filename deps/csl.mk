ifneq ($(USE_BINARYBUILDER_CSL),0)
CSL_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/CompilerSupportLibraries_jll.jl/releases/download/CompilerSupportLibraries-v$(CSL_VER)+$(CSL_BB_REL)
CSL_BB_NAME := CompilerSupportLibraries.v$(CSL_VER)
$(eval $(call bb-install,csl,CSL,true))
endif
