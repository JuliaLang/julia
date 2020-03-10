LLVM_VER_MAJ:=$(word 1, $(subst ., ,$(LLVM_VER)))
LLVM_VER_MIN:=$(word 2, $(subst ., ,$(LLVM_VER)))
# define a "short" LLVM version for easy comparisons
ifeq ($(LLVM_VER),svn)
LLVM_VER_SHORT:=svn
else
LLVM_VER_SHORT:=$(LLVM_VER_MAJ).$(LLVM_VER_MIN)
endif
LLVM_VER_PATCH:=$(word 3, $(subst ., ,$(LLVM_VER)))
ifeq ($(LLVM_VER_PATCH),)
LLVM_VER_PATCH := 0
endif

#See #611 in Yggdrasil
ifeq ($(USE_BINARYBUILDER_LLVM),1)
LLVM_LIB_SUFFIX:=${LLVM_VER_MAJ}
else
LLVM_LIB_SUFFIX:=${LLVM_VER_MAJ}jl
endif
