TABLEGEN=../build-15/llvm-dialects-tblgen

JuliaDialect.h.inc: JuliaDialect.td
	$(TABLEGEN) -o $@ -gen-dialect-decls --dialect julia -I ../include $<

JuliaDialect.cpp.inc: JuliaDialect.td
	$(TABLEGEN) -o $@ -gen-dialect-defs --dialect julia -I ../include $<
