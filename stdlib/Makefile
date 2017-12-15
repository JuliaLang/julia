default: get

PKG3_VERSION := a554607c939ab73d89c3e7c6a3dd08fd870145e8
PKG3_URL := https://github.com/JuliaLang/Pkg3.jl

Pkg3:
	git clone $(PKG3_URL) $@
checkout-pkg3: Pkg3
	git -C $< checkout $(PKG3_VERSION)
get-pkg3:  checkout-pkg3
clean-pkg3:
	rm -rf Pkg3

get: get-pkg3
clean: clean-pkg3

.PHONY: default get clean
