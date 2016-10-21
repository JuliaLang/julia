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

ifeq ($(LLVM_VER_SHORT),$(filter $(LLVM_VER_SHORT),3.3 3.4 3.5 3.6 3.7))
LLVM_USE_CMAKE := 0
else
LLVM_USE_CMAKE := 1
endif
