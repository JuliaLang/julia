!INCLUDE <..\Windows.inc>

.SUFFIXES: .c

NAME = julia

HEADERS = \
	builtin_proto.h \
	file_constants.h  \
	jltypes_internal.h  \
	julia.h  \
	newobj_internal.h  \
	os_detect.h  \
	uv_constants.h

OBJECTS = \
	jltypes.obj \
	gf.obj \
	ast.obj \
	builtins.obj \
	module.obj \
	codegen.obj \
	interpreter.obj \
	alloc.obj \
	dlload.obj \
	sys.obj \
	init.obj \
	task.obj \
	array.obj \
	dump.obj \
	toplevel.obj \
	jl_uv.obj \
	jlapi.obj \
	llvm-simdloop.obj \
	gc.obj

LIBFLISP = flisp\libflisp.lib
LIBSUPPORT = support\libsupport.lib
LIBUV = ..\deps\libuv\libuv.lib
FLISP = flisp\flisp.exe

INCLUDE = $(INCLUDE);$(MAKEDIR)\..\deps\libuv\include;$(MAKEDIR)\flisp;$(MAKEDIR)\support;C:\Program Files\llvm\include
!ifdef JL_DEBUG_BUILD
LIB = $(LIB);C:\Program Files\llvm\lib\Debug
!else
LIB = $(LIB);C:\Program Files\llvm\lib\Release
!endif

CFLAGS = $(CFLAGS) -D_CRT_SECURE_NO_WARNINGS
CFLAGS = $(CFLAGS) -DJL_SYSTEM_IMAGE_PATH=\"../lib/julia/sys.ji\" -DLIBRARY_EXPORTS

LIBWINDOWS = \
	kernel32.lib \
	ws2_32.lib \
	psapi.lib \
	advapi32.lib \
	iphlpapi.lib \
	shell32.lib

LIBLLVM = \
	LLVMAnalysis.lib \
	LLVMArchive.lib \
	LLVMAsmParser.lib \
	LLVMAsmPrinter.lib \
	LLVMBitReader.lib \
	LLVMBitWriter.lib \
	LLVMCodeGen.lib \
	LLVMCore.lib \
	LLVMDebugInfo.lib \
	LLVMExecutionEngine.lib \
	LLVMInstCombine.lib \
	LLVMInstrumentation.lib \
	LLVMInterpreter.lib \
	LLVMipa.lib \
	LLVMipo.lib \
	LLVMJIT.lib \
	LLVMLinker.lib \
	LLVMMC.lib \
	LLVMMCDisassembler.lib \
	LLVMMCJIT.lib \
	LLVMMCParser.lib \
	LLVMObject.lib \
	LLVMRuntimeDyld.lib \
	LLVMScalarOpts.lib \
	LLVMSelectionDAG.lib \
	LLVMSupport.lib \
	LLVMTableGen.lib \
	LLVMTarget.lib \
	LLVMTransformUtils.lib \
	LLVMVectorize.lib \
	LLVMX86AsmParser.lib \
	LLVMX86AsmPrinter.lib \
	LLVMX86CodeGen.lib \
	LLVMX86Desc.lib \
	LLVMX86Disassembler.lib \
	LLVMX86Info.lib \
	LLVMX86Utils.lib

default: lib$(NAME).dll

lib$(NAME).dll: $(OBJECTS) flisp\libflisp.lib support\libsupport.lib ..\deps\libuv\libuv.lib
	$(LINK) $(LFLAGS) /DLL $(LIBWINDOWS) $(LIBLLVM) /OUT:lib$(NAME).dll $**

$(FLISP) $(LIBFLISP):
	PUSHD flisp && $(MAKE) /NOLOGO /F Windows.mk && POPD

$(LIBSUPPORT):
	PUSHD support && $(MAKE) /NOLOGO /F Windows.mk && POPD

$(LIBUV):
	PUSHD ..\deps\libuv && $(MAKE) /NOLOGO /F Windows.mk && POPD

ast.obj: julia_flisp.boot.inc flisp\*.h

julia_flisp.boot.inc: julia_flisp.boot $(FLISP)
	.\flisp\flisp .\bin2hex.scm <julia_flisp.boot >$@

julia_flisp.boot: julia-parser.scm julia-syntax.scm match.scm utils.scm jlfrontend.scm mk_julia_flisp_boot.scm $(FLISP)
	.\flisp\flisp .\mk_julia_flisp_boot.scm

.c.obj:
	$(CC) $(CFLAGS) $<

.cpp.obj:
	$(CC) $(CFLAGS) $<

# vim: noexpandtab:ts=4:sw=4:

