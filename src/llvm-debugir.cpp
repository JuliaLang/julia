//===--- DebugIR.cpp - Transform debug metadata to allow debugging IR -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See THIRDPARTY.MD for details.
//
//===----------------------------------------------------------------------===//
//
// A Module transform pass that emits a succinct version of the IR and replaces
// the source file metadata to allow debuggers to step through the IR.
//
// FIXME: instead of replacing debug metadata, this pass should allow for
// additional metadata to be used to point capable debuggers to the IR file
// without destroying the mapping to the original source file.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <string>

#include "llvm-version.h"
#include "llvm-codegen-shared.h"

using namespace llvm;

#define DEBUG_TYPE "debug-ir"

namespace {

/// Builds a map of Value* to line numbers on which the Value appears in a
/// textual representation of the IR by plugging into the AssemblyWriter by
/// masquerading as an AssemblyAnnotationWriter.
class ValueToLineMap : public AssemblyAnnotationWriter {
  ValueMap<const Value *, unsigned int> Lines;
  typedef ValueMap<const Value *, unsigned int>::const_iterator LineIter;

  void addEntry(const Value *V, formatted_raw_ostream &Out) {
    Out.flush();
    Lines.insert(std::make_pair(V, Out.getLine() + 1));
  }

public:
  /// Prints Module to a null buffer in order to build the map of Value pointers
  /// to line numbers.
  ValueToLineMap(const Module *M) {
    raw_null_ostream ThrowAway;
    M->print(ThrowAway, this);
  }

  // This function is called after an Instruction, GlobalValue, or GlobalAlias
  // is printed.
  void printInfoComment(const Value &V, formatted_raw_ostream &Out) override {
    addEntry(&V, Out);
  }

  void emitBasicBlockStartAnnot(const BasicBlock *B,
                                formatted_raw_ostream &Out) override {
    addEntry(B, Out);
  }

  void emitFunctionAnnot(const Function *F,
                         formatted_raw_ostream &Out) override {
    addEntry(F, Out);
  }

  /// If V appears on a line in the textual IR representation, sets Line to the
  /// line number and returns true, otherwise returns false.
  bool getLine(const Value *V, unsigned int &Line) const {
    LineIter i = Lines.find(V);
    if (i != Lines.end()) {
      Line = i->second;
      return true;
    }
    return false;
  }
};

/// Updates debug metadata in a Module:
///   - changes Filename/Directory to values provided on construction
///   - adds/updates line number (DebugLoc) entries associated with each
///     instruction to reflect the instruction's location in an LLVM IR file
class DIUpdater : public InstVisitor<DIUpdater> {
  /// Builder of debug information
  DIBuilder Builder;

  /// Helper for type attributes/sizes/etc
  DataLayout Layout;

  /// Map of Value* to line numbers
  const ValueToLineMap LineTable;

  /// Map of Value* (in original Module) to Value* (in optional cloned Module)
  const ValueToValueMapTy *VMap;

  /// Directory of debug metadata
  DebugInfoFinder Finder;

  /// Source filename and directory
  StringRef Filename;
  StringRef Directory;

  // CU nodes needed when creating DI subprograms
  DIFile *FileNode;
  DILexicalBlockFile *LexicalBlockFileNode;

  Module &M;
  int tempNameCounter;

  ValueMap<const Function *, DISubprogram *> SubprogramDescriptors;
  ValueMap<const BasicBlock *, DILexicalBlock *> BlockDescriptors;
  DenseMap<const Type *, DIType *> TypeDescriptors;

public:
  DIUpdater(Module &M, StringRef Filename = StringRef(),
            StringRef Directory = StringRef(), const Module *DisplayM = nullptr,
            const ValueToValueMapTy *VMap = nullptr)
      : Builder(M), Layout(&M), LineTable(DisplayM ? DisplayM : &M), VMap(VMap),
        Finder(), Filename(Filename), Directory(Directory), FileNode(nullptr),
        LexicalBlockFileNode(nullptr), M(M), tempNameCounter(0) {

    // Even without finder, this screws up.
    Finder.processModule(M);
    visit(&M);
  }

  ~DIUpdater() { Builder.finalize(); }

  void visitModule(Module &M) {
    (void)M;
    if (Finder.compile_unit_count() > 1)
      report_fatal_error("DebugIR pass supports only a signle compile unit per "
                         "Module.");
    createCompileUnit(Finder.compile_unit_count() == 1
                          ? (DICompileUnit *)*Finder.compile_units().begin()
                          : nullptr);
  }

  void visitFunction(Function &F) {
    if (F.isDeclaration() || findDISubprogram(&F))
      return;

    StringRef MangledName = F.getName();
    DISubroutineType *Sig = createFunctionSignature(&F);

    // find line of function declaration
    unsigned Line = 0;
    if (!findLine(&F, Line)) {
      LLVM_DEBUG(dbgs() << "WARNING: No line for Function " << F.getName().str()
                        << "\n");
      return;
    }

    Instruction *FirstInst = &*F.begin()->begin();
    unsigned ScopeLine = 0;
    if (!findLine(FirstInst, ScopeLine)) {
      LLVM_DEBUG(dbgs() << "WARNING: No line for 1st Instruction in Function "
                        << F.getName().str() << "\n");
      return;
    }

    bool IsOptimized = false;

    DISubprogram::DISPFlags SPFlags = DISubprogram::SPFlagDefinition;
    if (IsOptimized)
      SPFlags |= DISubprogram::SPFlagOptimized;
    if (F.hasPrivateLinkage() || F.hasInternalLinkage())
      SPFlags |= DISubprogram::SPFlagLocalToUnit;
    if (F.isDeclaration())
      SPFlags |= DISubprogram::SPFlagDefinition;
    DISubprogram *Sub =
        Builder.createFunction(FileNode, F.getName(), MangledName, FileNode,
                               Line, Sig, ScopeLine, DINode::FlagZero, SPFlags);
    F.setSubprogram(Sub);
    LLVM_DEBUG(dbgs() << "create subprogram mdnode " << *Sub << ": "
                      << "\n");

    SubprogramDescriptors.insert(std::make_pair(&F, Sub));

    // Clang and the Kaleidoscope tutorial both copy function arguments to
    // allocas and then insert debug locations on these allocas.
    IRBuilder<> ArgIrBuilder(&F.getEntryBlock(),
                             F.getEntryBlock().getFirstInsertionPt());
    for (size_t I = 0; I < F.arg_size(); I++) {
      auto *Arg = F.getArg(I);
      if (Arg->getName().empty())
        continue;
      auto *Alloca =
          ArgIrBuilder.CreateAlloca(Arg->getType(), nullptr, Arg->getName());
      ArgIrBuilder.CreateStore(Arg, Alloca);

      // Scope must be the function for gdb to recognize this as a function
      // argument
      auto DILV = Builder.createParameterVariable(
          Sub, Arg->getName(), I + 1, FileNode, Line,
          getOrCreateType(Arg->getType()), true);
      auto Loc = DebugLoc(DILocation::get(M.getContext(), Line, 0, Sub));
      Builder.insertDeclare(Alloca, DILV, Builder.createExpression(), Loc.get(),
                            &F.getEntryBlock());
    }
  }

  void visitInstruction(Instruction &I) {
    DebugLoc Loc(I.getDebugLoc());

    /// If a ValueToValueMap is provided, use it to get the real instruction as
    /// the line table was generated on a clone of the module on which we are
    /// operating.
    Value *RealInst = nullptr;
    if (VMap)
      RealInst = VMap->lookup(&I);

    if (!RealInst)
      RealInst = &I;

    unsigned Col = 0; // FIXME: support columns
    unsigned Line;
    if (!LineTable.getLine(RealInst, Line)) {
      // Instruction has no line, it may have been removed (in the module that
      // will be passed to the debugger) so there is nothing to do here.
      LLVM_DEBUG(dbgs() << "WARNING: no LineTable entry for instruction "
                        << RealInst << "\n");
      return;
    }

    DILocalScope *Scope;
    DILocation *InlinedAt;
    if (Loc) {
      Scope = llvm::cast<DILocalScope>(Loc.getScope());
      InlinedAt = Loc.getInlinedAt();
    } else if ((Scope = dyn_cast<DILocalScope>(findScope(&I)))) {
      InlinedAt = nullptr;
    } else {
      LLVM_DEBUG(dbgs() << "WARNING: no valid scope for instruction " << &I
                        << ". no DebugLoc will be present."
                        << "\n");
      return;
    }

    if (isa<PHINode>(I))
      Scope = Scope->getSubprogram(); // See https://github.com/llvm/llvm-project/issues/118883

    DebugLoc NewLoc =
        DebugLoc(DILocation::get(M.getContext(), Line, Col, Scope, InlinedAt));
    addDebugLocation(I, NewLoc);

    if (!I.getType()->isVoidTy() && !I.getName().empty()) {
      auto DILV = Builder.createAutoVariable(Scope, I.getName(), FileNode, Line,
                                             getOrCreateType(I.getType()));
      if (isa<PHINode>(I))
        Builder.insertDbgValueIntrinsic(&I, DILV, Builder.createExpression(),
                                        NewLoc.get(), I.getParent()->getFirstNonPHI());
      else if (Instruction *NI = I.getNextNonDebugInstruction(/* SkipPseudoOp */ true))
        Builder.insertDbgValueIntrinsic(&I, DILV, Builder.createExpression(),
                                        NewLoc.get(), NI);
      else
        Builder.insertDbgValueIntrinsic(&I, DILV, Builder.createExpression(),
                                        NewLoc.get(), I.getParent());
    }
  }

private:
  void createCompileUnit(DICompileUnit *CUToReplace) {
    std::string Flags;
    bool IsOptimized = false;
    StringRef Producer;
    unsigned RuntimeVersion(0);
    StringRef SplitName;

    if (CUToReplace) {
      // save fields from existing CU to re-use in the new CU
      Producer = CUToReplace->getProducer();
      IsOptimized = CUToReplace->isOptimized();
      Flags = CUToReplace->getFlags().str();
      RuntimeVersion = CUToReplace->getRuntimeVersion();
      SplitName = CUToReplace->getSplitDebugFilename();
    } else {
      Producer =
          "LLVM Version " XSTR(LLVM_VERSION_MAJOR) "." XSTR(LLVM_VERSION_MINOR);
    }

    FileNode = Builder.createFile(Filename, Directory);
    DICompileUnit *CU =
        Builder.createCompileUnit(dwarf::DW_LANG_C99, FileNode, Producer,
                                  IsOptimized, Flags, RuntimeVersion);

    NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.cu");
    NMD->clearOperands();
    NMD->addOperand(CU);

    for (DISubprogram *S : Finder.subprograms()) {
      S->replaceUnit(CU);
      S->replaceOperandWith(0, FileNode); // replace 'File'
    }
  }

  DIScope *getBlockScope(DIScope *ParentScope, const BasicBlock *B) {
    auto BScope = BlockDescriptors.find(B);
    if (BScope != BlockDescriptors.end()) {
      return BScope->second;
    } else {
      // Let's build a scope for this block.
      unsigned Line = 0;
      if (!findLine(B, Line)) {
        LLVM_DEBUG(dbgs() << "WARNING: No line for basic block "
                          << B->getName().str() << " in Function "
                          << B->getParent()->getName().str() << "\n");
      }
      auto Scope = Builder.createLexicalBlock(ParentScope, FileNode, Line, 0);
      BlockDescriptors[B] = Scope;
      return Scope;
    }
  }

  /// Returns the MDNode* that represents the DI scope to associate with I
  DIScope *findScope(const Instruction *I) {

    const BasicBlock *B = I->getParent();
    const Function *F = B->getParent();

    auto returnFallback = [this, I]() {
      (void)I;
      LLVM_DEBUG(dbgs() << "WARNING: Using fallback lexical block file scope "
                        << LexicalBlockFileNode << " as scope for instruction "
                        << I << "\n");
      return LexicalBlockFileNode;
    };

    DISubprogram *SubprogramScope = findDISubprogram(F);
    if (!SubprogramScope)
      return returnFallback();

    auto *EntryBlockScope = getBlockScope(SubprogramScope, B);
    if (&F->getEntryBlock() == B) {
      return EntryBlockScope;
    }
    return getBlockScope(EntryBlockScope, B);
  }

  /// Returns the MDNode* that is the descriptor for F
  DISubprogram *findDISubprogram(const Function *F) {
    typedef ValueMap<const Function *, DISubprogram *>::const_iterator
        FuncNodeIter;
    FuncNodeIter i = SubprogramDescriptors.find(F);
    if (i != SubprogramDescriptors.end())
      return i->second;

    LLVM_DEBUG(dbgs() << "searching for DI scope node for Function " << F
                      << " in a list of " << Finder.subprogram_count()
                      << " subprogram nodes"
                      << "\n");

    // TODO: When would this even be reached?
    for (DISubprogram *S : Finder.subprograms()) {
      // TODO: Is describes correct?
      if (S->describes(F)) {
        LLVM_DEBUG(dbgs() << "Found DISubprogram " << S << " for function "
                          << F->getName() << "\n");
        return S;
      }
    }
    LLVM_DEBUG(dbgs() << "unable to find DISubprogram node for function "
                      << F->getName().str() << "\n");
    return nullptr;
  }

  /// Sets Line to the line number on which V appears and returns true. If a
  /// line location for V is not found, returns false.
  bool findLine(const Value *V, unsigned &Line) {
    if (LineTable.getLine(V, Line))
      return true;

    if (VMap) {
      Value *mapped = VMap->lookup(V);
      if (mapped && LineTable.getLine(mapped, Line))
        return true;
    }
    return false;
  }

  std::string getTypeName(Type *T) {
    std::string TypeName;
    raw_string_ostream TypeStream(TypeName);
    if (T)
      T->print(TypeStream);
    else
      TypeStream << "Printing <null> Type";
    TypeStream.flush();
    return TypeName;
  }

  /// Returns the MDNode that represents type T if it is already created, or 0
  /// if it is not.
  DIType *getType(const Type *T) {
    typedef DenseMap<const Type *, DIType *>::const_iterator TypeNodeIter;
    TypeNodeIter i = TypeDescriptors.find(T);
    if (i != TypeDescriptors.end())
      return i->second;
    return nullptr;
  }

  /// Returns a DebugInfo type from an LLVM type T.
  DIType *getOrCreateType(Type *T) {
    DIType *N = getType(T);
    if (N)
      return N;
    else if (T->isVoidTy())
      return Builder.createUnspecifiedType("void");
    else if (T->isStructTy()) {
      // NOTE: where does DINodeArray come from?
      StructType *ST = cast<StructType>(T);
      if (ST->isOpaque())
        N = Builder.createUnspecifiedType(ST->getName());
      else {
        DICompositeType *S = Builder.createStructType(
            LexicalBlockFileNode,
            ST->hasName() ? T->getStructName() : "literal", FileNode,
            /*LineNumber=*/0, Layout.getTypeSizeInBits(T),
            Layout.getPrefTypeAlign(T).value() * CHAR_BIT, /*DIFlags=*/llvm::DINode::FlagZero,
            /*DerivedFrom=*/nullptr, llvm::DINodeArray()); // filled in later
        N = S; // the Node _is_ the struct type.

        // N is added to the map (early) so that element search below can find
        // it, so as to avoid infinite recursion for structs that contain
        // pointers to their own type.
        TypeDescriptors[T] = N;

        SmallVector<Metadata *, 4>
            Elements; // unfortunately, SmallVector<Type *> does not decay to
                      // SmallVector<Metadata *>

        auto *TLayout = Layout.getStructLayout(llvm::cast<StructType>(T));
        for (unsigned I = 0; I < T->getStructNumElements(); ++I) {
          Type *ElType = T->getStructElementType(I);
          DIType *ElDIType = getOrCreateType(ElType);
          DIType *MemType = Builder.createMemberType(
              LexicalBlockFileNode,
              (ST->hasName() ? T->getStructName().str() + "." +
                                   std::to_string(tempNameCounter++)
                             : "literal"),
              FileNode, 0, 0, 0, TLayout->getElementOffsetInBits(I),
              DINode::DIFlags::FlagZero, ElDIType);
          Elements.push_back(MemType);
        }

        Builder.replaceArrays(S, Builder.getOrCreateArray(Elements));
      }
    } else if (T->isPointerTy()) {
      N = Builder.createPointerType(
          nullptr, Layout.getPointerTypeSizeInBits(T),
          Layout.getPrefTypeAlign(T).value() * CHAR_BIT,
#if LLVM_VERSION_MAJOR > 15
          /*DWARFAddressSpace=*/std::nullopt,
#else
          /*DWARFAddressSpace=*/None,
#endif
          getTypeName(T));
    } else if (T->isArrayTy()) {
      SmallVector<Metadata *, 4>
          Subscripts; // unfortunately, SmallVector<Type *> does not decay to
                      // SmallVector<Metadata *>

      Subscripts.push_back(
          Builder.getOrCreateSubrange(0, T->getArrayNumElements() - 1));

      N = Builder.createArrayType(Layout.getTypeSizeInBits(T),
                                  Layout.getPrefTypeAlign(T).value() * CHAR_BIT,
                                  getOrCreateType(T->getArrayElementType()),
                                  Builder.getOrCreateArray(Subscripts));
    } else {
      int encoding = llvm::dwarf::DW_ATE_signed;
      if (T->isIntegerTy())
        encoding = llvm::dwarf::DW_ATE_unsigned;
      else if (T->isFloatingPointTy())
        encoding = llvm::dwarf::DW_ATE_float;

      N = Builder.createBasicType(getTypeName(T), T->getPrimitiveSizeInBits(),
                                  encoding);
    }
    TypeDescriptors[T] = N;
    return N;
  }

  /// Returns a DebugInfo type that represents a function signature for Func.
  DISubroutineType *createFunctionSignature(const Function *Func) {
    SmallVector<Metadata *, 4> Params; // SmallVector<DIType *> does not
                                       // auto-case to SmallVector<Metadata *>
    DIType *ReturnType = getOrCreateType(Func->getReturnType());
    Params.push_back(ReturnType);

    for (const Argument &Arg : Func->args()) {
      Type *T = Arg.getType();
      Params.push_back(getOrCreateType(T));
    }

    DITypeRefArray ParamArray = Builder.getOrCreateTypeArray(Params);
    return Builder.createSubroutineType(ParamArray);
  }

  /// Associates Instruction I with debug location Loc.
  void addDebugLocation(Instruction &I, DebugLoc Loc) { I.setDebugLoc(Loc); }
};

} // anonymous namespace

namespace debugir {

std::unique_ptr<Module> createDebugInfo(Module &M, std::string Directory,
                                        std::string Filename) {

  auto VMap = std::make_unique<ValueToValueMapTy>();
  auto DisplayM = CloneModule(M, *VMap);
  StripDebugInfo(*(DisplayM.get()));

  {
    // DIUpdater is in its own scope so that it's destructor, and hence
    // DIBuilder::finalize() gets called. Without that there's dangling stuff.
    DIUpdater R(M, Filename, Directory, DisplayM.get(), VMap.get());
  }

  auto DIVersionKey = "Debug Info Version";
  if (!M.getModuleFlag(DIVersionKey))
    // Add the current debug info version into the module.
    M.addModuleFlag(Module::Warning, DIVersionKey, DEBUG_METADATA_VERSION);

  assert(!verifyModule(M, &errs()) && "verifyModule found issues");

  return DisplayM;
}

} // namespace llvm
