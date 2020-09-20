## nghttp2

ifneq ($(USE_BINARYBUILDER_NGHTTP2), 1)
ifneq ($(USE_SYSTEM_NGHTTP2), 1)
$(error "FIXME: Source build nghttp2 is not supported.")
endif
endif

NGHTTP2_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/nghttp2_jll.jl/releases/download/nghttp2-v$(NGHTTP2_VER)+$(NGHTTP2_BB_REL)
NGHTTP2_BB_NAME := nghttp2.v$(NGHTTP2_VER)

$(eval $(call bb-install,nghttp2,NGHTTP2,false))
