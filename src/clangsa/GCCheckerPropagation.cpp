// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "GCChecker.h"

#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/SmallPtrSet.h"

// GCChecker root propagation, safepoint handling, and checker callbacks.

namespace jl_gc_checker {

bool GCChecker::gcEnabledHere(CheckerContext &C) const {
  return gcEnabledHere(C.getState());
}

bool GCChecker::gcEnabledHere(ProgramStateRef State) const {
  unsigned disabledAt = State->get<GCDisabledAt>();
  return disabledAt == (unsigned)-1;
}

bool GCChecker::safepointEnabledHere(CheckerContext &C) const {
  return safepointEnabledHere(C.getState());
}

bool GCChecker::safepointEnabledHere(ProgramStateRef State) const {
  unsigned disabledAt = State->get<SafepointDisabledAt>();
  return disabledAt == (unsigned)-1;
}

bool GCChecker::propagateArgumentRootedness(CheckerContext &C,
                                            ProgramStateRef &State) const {
  const auto *LCtx = C.getLocationContext();

  const auto *Site = cast<StackFrameContext>(LCtx)->getCallSite();
  if (!Site)
    return false;

  const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());
  if (!FD)
    return false;

  const auto *CE = dyn_cast<CallExpr>(Site);
  if (!CE)
    return false;

  // FD->dump();

  bool Change = false;
  unsigned idx = 0;
  for (const auto P : FD->parameters()) {
    unsigned ArgIdx = idx++;
    bool IsRootSlotParam =
        declHasAnnotation(P, "julia_require_rooted_slot");
    if (!IsRootSlotParam && !isGCObjectType(P->getType()))
      continue;

    auto Arg = State->getSVal(CE->getArg(ArgIdx), LCtx->getParent());
    auto Param = State->getLValue(P, LCtx);
    if (IsRootSlotParam) {
      GCObjectSet RootObjects = emptyObjectSet(State);
      if (const MemRegion *Root = getRegionForPointerValue(Arg, C)) {
        Root = Root->StripCasts();
        State = State->set<GCRootMap>(Root, -1);
        State = addPermanentRoot(State, Root);
        State = State->add<GCConservativeRootRegions>(Root);
        State = bindRootRegionToCurrentValue(
            State, Root, P->getType()->getPointeeType(), C);
        RootObjects = getObjectsForRegion(State, Root);
        Change = true;
      }
      SVal ParamValue =
          getOrConjurePointerValue(Param, P->getType(), State, C);
      if (const MemRegion *ParamRoot = getRegionForPointerValue(ParamValue, C)) {
        ParamRoot = ParamRoot->StripCasts();
        State = State->set<GCRootMap>(ParamRoot, -1);
        State = addPermanentRoot(State, ParamRoot);
        State = State->add<GCConservativeRootRegions>(ParamRoot);
        State = bindRegionToObjects(State, ParamRoot, RootObjects, C);
        Change = true;
      }
      continue;
    }

    const Expr *ArgExpr = CE->getArg(ArgIdx);
    const Expr *ValueExpr = ignoreOuterPointerCasts(ArgExpr);
    GCObjectSet ArgObjects = getObjectsForExprValue(ValueExpr, Arg, State, C);
    if (ArgObjects.isEmpty()) {
      if (const MemRegion *RootRegion =
              getRootingRegionForExpr(ValueExpr, State, C)) {
        State = bindRootRegionToCurrentValue(
            State, RootRegion, ArgExpr ? ArgExpr->getType() : QualType(), C);
        ArgObjects = getObjectsForRegion(State, RootRegion);
      }
    }
    std::optional<LivenessState> ValS;
    if (!ArgObjects.isEmpty())
      ValS = aggregateObjectState(State, ArgObjects);
    SymbolRef ArgSym = walkToRoot(
        [](SymbolRef Sym, const LivenessState *OldVState) { return !OldVState; },
        State, Arg.getAsRegion());
    if (!ValS && !ArgSym) {
      continue;
    }
    if (!ValS)
      ValS = getStateForSymbol(State, ArgSym);
    if (!ValS) {
      report_error(
          [&](PathSensitiveBugReport *Report) {
            Report->addNote(
                "Tried to find root for this parameter in inlined call",
                PathDiagnosticLocation::create(P, C.getSourceManager()));
          },
          C, "Missed allocation of parameter");
      continue;
    }
    SymbolRef ParamSym = State->getSVal(Param).getAsSymbol(true);
    if (!ParamSym) {
      continue;
    }
    if (isGloballyRootedType(P->getType())) {
      GCObject Object = ensureObjectForSymbol(ParamSym, State, C);
      if (Object)
        State = addPermanentRoot(State, getObjectRegion(Object, C));
      Change = true;
      continue;
    }
    if (ArgObjects.isEmpty()) {
      if (GCObject Object = ensureObjectForSymbol(ArgSym, State, C, *ValS))
        ArgObjects = singletonObjectSet(State, Object);
    }
    if (const MemRegion *ParamRegion = Param.getAsRegion())
      State = bindRegionToObjects(State, ParamRegion, ArgObjects, C);
    State = setObjectsForSymbol(State, ParamSym, ArgObjects, C);
    Change = true;
  }
  return Change;
}

void GCChecker::checkBeginFunction(CheckerContext &C) const {
  // Consider top-level argument values rooted, unless an annotation says
  // otherwise
  const auto *LCtx = C.getLocationContext();
  const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());
  assert(FD);
  unsigned CurrentHeight = getStackFrameHeight(C.getStackFrame());
  ProgramStateRef State = C.getState();
  bool Change = false;
  if (C.inTopFrame()) {
    State = State->set<GCDisabledAt>((unsigned)-1);
    State = State->set<SafepointDisabledAt>((unsigned)-1);
    Change = true;
  }
  if (gcEnabledHere(State) && declHasAnnotation(FD, "julia_gc_disabled")) {
    State = State->set<GCDisabledAt>(CurrentHeight);
    Change = true;
  }
  bool isFunctionSafepoint = !isFDAnnotatedNotSafepoint(FD, getSM(C));
  if (safepointEnabledHere(State) &&
      (!isFunctionSafepoint || declHasAnnotation(FD, "julia_notsafepoint_leave"))) {
    State = State->set<SafepointDisabledAt>(CurrentHeight);
    Change = true;
  }
  if (!C.inTopFrame()) {
    if (propagateArgumentRootedness(C, State) || Change)
      C.addTransition(State);
    return;
  }
  for (const auto P : FD->parameters()) {
    if (declHasAnnotation(P, "julia_require_rooted_slot")) {
      auto Param = State->getLValue(P, LCtx);
      SVal ParamValue =
          getOrConjurePointerValue(Param, P->getType(), State, C);
      const MemRegion *Root = getRegionForPointerValue(ParamValue, C);
      if (Root) {
        Root = Root->StripCasts();
        State = State->set<GCRootMap>(Root, -1);
        State = addPermanentRoot(State, Root);
        State = State->add<GCConservativeRootRegions>(Root);
        State = bindRootRegionToCurrentValue(
            State, Root, P->getType()->getPointeeType(), C);
        Change = true;
      }
    } else if (isGCObjectType(P->getType())) {
      auto Param = State->getLValue(P, LCtx);
      SVal AssignedValue = State->getSVal(Param);
      SymbolRef AssignedSym = AssignedValue.getAsSymbol(true);
      if (!AssignedSym)
        continue;
      assert(AssignedSym);
      LivenessState ArgState = LivenessState::getForArgument(FD, P, isFunctionSafepoint);
      bool ArgIsRooted =
          isFunctionSafepoint && !declHasAnnotation(P, "julia_maybe_unrooted");
      GCObject Object = ensureObjectForSymbol(AssignedSym, State, C, ArgState);
      if (Object) {
        const MemRegion *ObjectRegion = getObjectRegion(Object, C);
        if (ArgIsRooted) {
          State = addPermanentRoot(State, ObjectRegion);
          State = setObjectsForRegion(State, ObjectRegion,
                                      singletonObjectSet(State, Object), C);
          if (const MemRegion *ArgumentRegion = AssignedValue.getAsRegion()) {
            State = addPermanentRoot(State, ArgumentRegion);
            State = setObjectsForRegion(State, ArgumentRegion,
                                        singletonObjectSet(State, Object), C);
          }
        }
        if (isGloballyRootedType(P->getType())) {
          State = addPermanentRoot(State, ObjectRegion);
        }
      }
      Change = true;
    }
  }
  if (Change) {
    C.addTransition(State);
  }
}

void GCChecker::checkEndFunction(const clang::ReturnStmt *RS,
                                 CheckerContext &C) const {
  ProgramStateRef State = C.getState();
  const auto *LCtx = C.getLocationContext();
  const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());

  if (RS && gcEnabledHere(State) && RS->getRetValue() && isGCTracked(RS->getRetValue())) {
    auto ResultVal = C.getSVal(RS->getRetValue());
    SymbolRef Sym = ResultVal.getAsSymbol(true);
    std::optional<LivenessState> ValS = getStateForSymbol(State, Sym);
    GCObjectSet Objects = getObjectsForSymbol(State, Sym);
    if ((ValS && ValS->isPotentiallyFreed()) ||
        objectsMayBeFreed(State, Objects)) {
      report_value_error(C, Sym, "Return value may have been GCed",
                         RS->getSourceRange());
    }
  }

  unsigned CurrentHeight = getStackFrameHeight(C.getStackFrame());
  bool Changed = false;
  if (State->get<GCDisabledAt>() == CurrentHeight) {
    State = State->set<GCDisabledAt>((unsigned)-1);
    Changed = true;
  }
  if (State->get<SafepointDisabledAt>() == CurrentHeight) {
    if (!isFDAnnotatedNotSafepoint(FD, getSM(C)) && !(FD && declHasAnnotation(FD, "julia_notsafepoint_enter"))) {
      report_error(C, "Safepoints disabled at end of function");
    }
    State = State->set<SafepointDisabledAt>((unsigned)-1);
    Changed = true;
  }
  if (Changed)
    C.addTransition(State);
  if (!C.inTopFrame())
    return;
  unsigned CurrentDepth = C.getState()->get<GCDepth>();
  if (CurrentDepth != 0) {
    report_error(C, "Non-popped GC frame present at end of function");
  }
}

// Clang does not propagate `annotate` attributes from an overridden base-class
// virtual method onto a derived-class override (overrides are distinct
// declarations, not redeclarations of the same entity, so attribute merging
// never runs across them). The Julia annotations on a virtual method express a
// contract that any override is expected to honor, so when scanning a
// CXXMethodDecl we also visit every method it transitively overrides.
//
// `Visit` is invoked on `D` and each transitively overridden method until it
// returns a value that tests as true, which becomes the result; otherwise the
// last (false) value is returned.
template <typename Fn>
static auto forDeclAndOverridden(const clang::Decl *D, Fn Visit)
    -> decltype(Visit(D)) {
  decltype(Visit(D)) Result = Visit(D);
  if (Result)
    return Result;
  const auto *MD = dyn_cast_or_null<CXXMethodDecl>(D);
  if (!MD)
    return Result;
  llvm::SmallVector<const CXXMethodDecl *, 4> Worklist(MD->overridden_methods());
  llvm::SmallPtrSet<const CXXMethodDecl *, 8> Seen;
  while (!Worklist.empty()) {
    const CXXMethodDecl *Cur = Worklist.pop_back_val();
    if (!Cur || !Seen.insert(Cur).second)
      continue;
    Result = Visit(Cur);
    if (Result)
      return Result;
    Worklist.append(Cur->overridden_methods().begin(),
                    Cur->overridden_methods().end());
  }
  return Result;
}

const AnnotateAttr *GCChecker::declHasAnnotation(const clang::Decl *D, const char *which) {
  if (!D)
    return nullptr;
  return forDeclAndOverridden(D, [&](const clang::Decl *Cur) -> const AnnotateAttr * {
    for (const auto *Ann : Cur->specific_attrs<AnnotateAttr>()) {
      if (Ann->getAnnotation() == which)
        return Ann;
    }
    return nullptr;
  });
}

std::optional<unsigned>
GCChecker::declHasIndexedAnnotation(const clang::Decl *D, StringRef Prefix) {
  if (!D)
    return std::nullopt;
  return forDeclAndOverridden(D, [&](const clang::Decl *Cur) -> std::optional<unsigned> {
    for (const auto *Ann : Cur->specific_attrs<AnnotateAttr>()) {
      StringRef Annotation = Ann->getAnnotation();
      if (!Annotation.consume_front(Prefix))
        continue;
      unsigned Index = 0;
      if (!Annotation.getAsInteger(10, Index))
        return Index;
    }
    return std::nullopt;
  });
}

std::optional<std::pair<unsigned, unsigned>>
GCChecker::declHasIndexedPairAnnotation(const clang::Decl *D,
                                        StringRef Prefix) {
  if (!D)
    return std::nullopt;
  return forDeclAndOverridden(
      D, [&](const clang::Decl *Cur) -> std::optional<std::pair<unsigned, unsigned>> {
        for (const auto *Ann : Cur->specific_attrs<AnnotateAttr>()) {
          StringRef Annotation = Ann->getAnnotation();
          if (!Annotation.consume_front(Prefix))
            continue;
          auto Parts = Annotation.split(':');
          unsigned First = 0;
          unsigned Second = 0;
          if (!Parts.first.getAsInteger(10, First) &&
              !Parts.second.getAsInteger(10, Second))
            return std::make_pair(First, Second);
        }
        return std::nullopt;
      });
}

bool GCChecker::isFDAnnotatedNotSafepoint(const clang::FunctionDecl *FD, const SourceManager &SM) {
  if (!FD)
      return false;
  if (declHasAnnotation(FD, "julia_not_safepoint"))
      return true;
  SourceLocation Loc = FD->getLocation();
  StringRef Name = SM.getFilename(Loc);
  Name = llvm::sys::path::filename(Name);
  if (Name.starts_with("llvm-"))
      return true;
  return false;
}

static bool isMutexLock(StringRef name) {
    return name == "uv_mutex_lock" ||
           name == "uv_mutex_trylock" ||
           name == "pthread_mutex_lock" ||
           name == "pthread_mutex_trylock" ||
           name == "__gthread_mutex_lock" ||
           name == "__gthread_mutex_trylock" ||
           name == "__gthread_recursive_mutex_lock" ||
           name == "__gthread_recursive_mutex_trylock" ||
           name == "pthread_spin_lock" ||
           name == "pthread_spin_trylock" ||
           name == "uv_rwlock_rdlock" ||
           name == "uv_rwlock_tryrdlock" ||
           name == "uv_rwlock_wrlock" ||
           name == "uv_rwlock_trywrlock" ||
           false;
}

static bool isMutexUnlock(StringRef name) {
    return name == "uv_mutex_unlock" ||
           name == "pthread_mutex_unlock" ||
           name == "__gthread_mutex_unlock" ||
           name == "__gthread_recursive_mutex_unlock" ||
           name == "pthread_spin_unlock" ||
           name == "uv_rwlock_rdunlock" ||
           name == "uv_rwlock_wrunlock" ||
           false;
}


bool GCChecker::isGCTrackedType(QualType QT) {
  return isJuliaType(
             [](StringRef Name) {
               if (Name.ends_with_insensitive("jl_value_t") ||
                   Name.ends_with_insensitive("jl_svec_t") ||
                   Name.ends_with_insensitive("jl_sym_t") ||
                   Name.ends_with_insensitive("jl_expr_t") ||
                   Name.ends_with_insensitive("jl_code_info_t") ||
                   Name.ends_with_insensitive("jl_array_t") ||
                   Name.ends_with_insensitive("jl_genericmemory_t") ||
                   Name.ends_with_insensitive("jl_genericmemoryref_t") ||
                   Name.ends_with_insensitive("jl_method_t") ||
                   Name.ends_with_insensitive("jl_method_instance_t") ||
                   Name.ends_with_insensitive("jl_debuginfo_t") ||
                   Name.ends_with_insensitive("jl_tupletype_t") ||
                   Name.ends_with_insensitive("jl_datatype_t") ||
                   Name.ends_with_insensitive("jl_typemap_entry_t") ||
                   Name.ends_with_insensitive("jl_typemap_level_t") ||
                   Name.ends_with_insensitive("jl_typename_t") ||
                   Name.ends_with_insensitive("jl_module_t") ||
                   Name.ends_with_insensitive("jl_tupletype_t") ||
                   Name.ends_with_insensitive("jl_gc_tracked_buffer_t") ||
                   Name.ends_with_insensitive("jl_binding_t") ||
                   Name.ends_with_insensitive("jl_binding_partition_t") ||
                   Name.ends_with_insensitive("jl_ordereddict_t") ||
                   Name.ends_with_insensitive("jl_tvar_t") ||
                   Name.ends_with_insensitive("jl_typemap_t") ||
                   Name.ends_with_insensitive("jl_unionall_t") ||
                   Name.ends_with_insensitive("jl_methtable_t") ||
                   Name.ends_with_insensitive("jl_methcache_t") ||
                   Name.ends_with_insensitive("jl_cgval_t") ||
                   Name.ends_with_insensitive("jl_codectx_t") ||
                   Name.ends_with_insensitive("jl_code_instance_t") ||
                   Name.ends_with_insensitive("jl_excstack_t") ||
                   Name.ends_with_insensitive("jl_task_t") ||
                   Name.ends_with_insensitive("jl_uniontype_t") ||
                   Name.ends_with_insensitive("jl_method_match_t") ||
                   Name.ends_with_insensitive("jl_vararg_t") ||
                   Name.ends_with_insensitive("jl_opaque_closure_t") ||
                   Name.ends_with_insensitive("jl_globalref_t") ||
                   Name.ends_with_insensitive("jl_abi_override_t") ||
                   Name.ends_with_insensitive("jl_ast_context_t") ||
                   // Probably not technically true for these, but let's allow it as a root
                   Name.ends_with_insensitive("jl_ircode_state") ||
                   Name.ends_with_insensitive("typemap_intersection_env") ||
                   Name.ends_with_insensitive("interpreter_state") ||
                   Name.ends_with_insensitive("jl_typeenv_t") ||
                   Name.ends_with_insensitive("jl_stenv_t") ||
                   Name.ends_with_insensitive("set_world") ||
                   Name.ends_with_insensitive("jl_codectx_t") ||
                   Name.ends_with_insensitive("jl_codegen_params_t") ||
                   Name.ends_with_insensitive("egal_set")) {
                 return true;
               }
               return false;
             },
             QT);
}

bool GCChecker::isGenericMemoryRefType(QualType QT) {
  if (QT.isNull())
    return false;
  QualType Canonical = QT.getCanonicalType().getUnqualifiedType();
  if (Canonical->isPointerType() || Canonical->isReferenceType() ||
      Canonical->isArrayType())
    return false;
  return isJuliaType(
      [](StringRef Name) {
        return Name.ends_with_insensitive("jl_genericmemoryref_t");
      },
      QT);
}

const FieldDecl *GCChecker::getGenericMemoryRefMemField(QualType QT) {
  if (!isGenericMemoryRefType(QT))
    return nullptr;
  QT = QT.getCanonicalType().getUnqualifiedType();
  const auto *RT = QT->getAs<RecordType>();
  if (!RT)
    return nullptr;
  const RecordDecl *RD = RT->getDecl()->getDefinition();
  if (!RD)
    RD = RT->getDecl();
  for (const FieldDecl *Field : RD->fields()) {
    if (Field->getName() == "mem")
      return Field;
  }
  return nullptr;
}

bool GCChecker::isGCObjectType(QualType QT) {
  if (!isGCTrackedType(QT))
    return false;
  QT = QT.getCanonicalType().getUnqualifiedType();
  if (!QT->isPointerType())
    return false;
  QualType Pointee =
      QT->getPointeeType().getCanonicalType().getUnqualifiedType();
  return !Pointee->isPointerType();
}

bool GCChecker::isGCTracked(const Expr *E) {
  while (1) {
    if (isGCTrackedType(E->getType()))
      return true;
    if (auto ICE = dyn_cast<ImplicitCastExpr>(E))
      E = ICE->getSubExpr();
    else if (auto CE = dyn_cast<CastExpr>(E))
      E = CE->getSubExpr();
    else
      return false;
  }
}

QualType GCChecker::getAtomicValueType(QualType Type) {
  if (const auto *AT = Type->getAs<AtomicType>())
    return AT->getValueType().getUnqualifiedType();
  return Type;
}

const Expr *GCChecker::ignoreOuterPointerCasts(const Expr *E) {
  if (!E)
    return nullptr;
  E = E->IgnoreParens();
  while (const auto *ICE = dyn_cast<ImplicitCastExpr>(E)) {
    switch (ICE->getCastKind()) {
    case CK_BitCast:
    case CK_LValueBitCast:
    case CK_NoOp:
      E = ICE->getSubExpr()->IgnoreParens();
      continue;
    default:
      return E;
    }
  }
  return E;
}

bool GCChecker::isAssignmentLHS(const Expr *E, CheckerContext &C) {
  if (!E)
    return false;
  const Expr *Target = E->IgnoreParenCasts();
  auto Parents = C.getASTContext().getParents(*E);
  while (!Parents.empty()) {
    if (const auto *BO = Parents[0].get<BinaryOperator>())
      return BO->isAssignmentOp() &&
             BO->getLHS()->IgnoreParenCasts() == Target;
    if (const auto *PO = Parents[0].get<ParenExpr>()) {
      Parents = C.getASTContext().getParents(*PO);
      continue;
    }
    if (const auto *CE = Parents[0].get<CastExpr>()) {
      Parents = C.getASTContext().getParents(*CE);
      continue;
    }
    return false;
  }
  return false;
}

bool GCChecker::isGloballyRootedType(QualType QT) const {
  return isJuliaType(
      [](StringRef Name) {
        return Name.ends_with("jl_sym_t") ||
               Name.ends_with("jl_ast_context_t");
      },
      QT);
}

bool GCChecker::isSafepoint(const CallEvent &Call, CheckerContext &C) const {
  bool isCalleeSafepoint = true;
  if (Call.isInSystemHeader()) {
    // defined by -isystem per
    // https://clang.llvm.org/docs/UsersManual.html#controlling-diagnostics-in-system-headers
    isCalleeSafepoint = false;
  } else {
    const clang::Decl *Decl = Call.getDecl(); // we might not have a simple call, or we might have an SVal
    const clang::Expr *Callee = nullptr;
    if (auto CE = dyn_cast_or_null<CallExpr>(Call.getOriginExpr())) {
      Callee = CE->getCallee();
      if (Decl == nullptr)
          Decl = CE->getCalleeDecl(); // ignores dyn_cast<FunctionDecl>, so it could also be a MemberDecl, etc.
    }
    const DeclContext *DC = Decl ? Decl->getDeclContext() : nullptr;
    while (DC) {
      // Anything in llvm or std is not a safepoint
      if (const NamespaceDecl *NDC = dyn_cast<NamespaceDecl>(DC))
        if (NDC->getName() == "llvm" || NDC->getName() == "std" || NDC->getName() == "tp")
          return false;
      DC = DC->getParent();
    }
    const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
    if (!Decl || !FD) {
      if (Callee == nullptr) {
        isCalleeSafepoint = true;
      } else if (const ElaboratedType *ET = dyn_cast<ElaboratedType>(Callee->getType())){
        if (const TypedefType *TDT = dyn_cast<TypedefType>(ET->getNamedType())) {
          isCalleeSafepoint =
              !declHasAnnotation(TDT->getDecl(), "julia_not_safepoint");
        }
      } else if (const CXXPseudoDestructorExpr *PDE =
                     dyn_cast<CXXPseudoDestructorExpr>(Callee)) {
        // A pseudo-destructor is an expression that looks like a member
        // access to a destructor of a scalar type. A pseudo-destructor
        // expression has no run-time semantics beyond evaluating the base
        // expression (which would have its own CallEvent, if applicable).
        isCalleeSafepoint = false;
      }
    } else if (FD) {
      StringRef FDName =
          FD->getDeclName().isIdentifier() ? FD->getName() : "";
      if (FDName == "sig_match_fast")
        isCalleeSafepoint = false;
      else if (FD->getBuiltinID() != 0 || FD->isTrivial())
        isCalleeSafepoint = false;
      else if ((FDName.starts_with("uv_") ||
                FDName.starts_with("unw_") ||
                FDName.starts_with("_U")) &&
               FDName != "uv_run")
        isCalleeSafepoint = false;
      else
        isCalleeSafepoint = !isFDAnnotatedNotSafepoint(FD, getSM(C));
    }
  }
  return isCalleeSafepoint;
}

bool GCChecker::processPotentialSafepoint(const CallEvent &Call,
                                          CheckerContext &C,
                                          ProgramStateRef &State) const {
  if (!isSafepoint(Call, C))
    return false;
  bool DidChange = false;
  if (!gcEnabledHere(C))
    return false;
  const Decl *D = Call.getDecl();
  const FunctionDecl *FD = D ? D->getAsFunction() : nullptr;
  GCObjectSet SpeciallyRootedObjects = emptyObjectSet(State);
  if (FD) {
    for (unsigned i = 0; i < FD->getNumParams(); ++i) {
      QualType ParmType = FD->getParamDecl(i)->getType();
      if (declHasAnnotation(FD->getParamDecl(i), "julia_temporarily_roots")) {
        if (ParmType->isPointerType() &&
            ParmType->getPointeeType()->isPointerType() &&
            isGCTrackedType(ParmType->getPointeeType())) {
          // This is probably an out parameter. Find the value it refers to now.
          SVal Loaded =
              State->getSVal(*(Call.getArgSVal(i).getAs<Loc>()));
          SpeciallyRootedObjects = getObjectsForSVal(State, Loaded);
          continue;
        }
        SVal Test = Call.getArgSVal(i);
        SpeciallyRootedObjects = getObjectsForSVal(State, Test);
        break;
      }
    }
  }

  // Don't free the return value
  SymbolRef RetSym = Call.getReturnValue().getAsSymbol();
  GCObjectSet RetObjects = getObjectsForSymbol(State, RetSym);
  GCObjectSet Reachable = computeReachableObjects(State);

  // Symbolically free all unrooted values.
  GCObjectStateMapTy AMap = State->get<GCObjectStateMap>();
  for (auto I = AMap.begin(), E = AMap.end(); I != E; ++I) {
    if (I.getData().isJustAllocated()) {
      if (objectSetContains(SpeciallyRootedObjects, I.getKey()))
        continue;
      if (objectSetContains(RetObjects, I.getKey()))
        continue;
      if (Reachable.contains(I.getKey()))
        continue;
      State = State->set<GCObjectStateMap>(I.getKey(), LivenessState::getFreed());
      DidChange = true;
    }
  }
  return DidChange;
}

bool GCChecker::processArgumentRooting(const CallEvent &Call, CheckerContext &C,
                                       ProgramStateRef &State) const {
  auto *Decl = Call.getDecl();
  const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
  if (!FD)
    return false;

  auto getRooterObjects = [&](unsigned RooterIdx) {
    if (RooterIdx >= Call.getNumArgs())
      return emptyObjectSet(State);
    SVal RootingSVal = Call.getArgSVal(RooterIdx);
    GCObjectSet Objects =
        getObjectsForExprValue(Call.getArgExpr(RooterIdx), RootingSVal, State,
                               C);
    if (Objects.isEmpty() && RooterIdx < FD->getNumParams()) {
      Objects = getObjectsForGenericMemoryRef(
          Call.getArgExpr(RooterIdx), RootingSVal,
          FD->getParamDecl(RooterIdx)->getType(), State, C);
    }
    if (Objects.isEmpty())
      Objects = getObjectsForRootPropagatingArgument(Call, RooterIdx, State, C);
    if (Objects.isEmpty()) {
      const Expr *RootingExpr = Call.getArgExpr(RooterIdx);
      RootingExpr = RootingExpr ? RootingExpr->IgnoreParenCasts() : nullptr;
      if (const auto *CE = dyn_cast_or_null<CallExpr>(RootingExpr)) {
        const FunctionDecl *InnerFD = CE->getDirectCallee();
        if (!InnerFD && CE->getCalleeDecl())
          InnerFD = dyn_cast<FunctionDecl>(CE->getCalleeDecl());
        if (InnerFD) {
          for (unsigned i = 0; i < InnerFD->getNumParams() &&
                               i < CE->getNumArgs(); ++i) {
            if (!declHasAnnotation(InnerFD->getParamDecl(i),
                                   "julia_propagates_root"))
              continue;
            const Expr *InnerArg = CE->getArg(i);
            SVal InnerSVal = C.getSVal(InnerArg);
            Objects = getObjectsForExprValue(InnerArg, InnerSVal, State, C);
            break;
          }
        }
      }
    }
    return Objects;
  };

  auto getRootingRegion = [&](unsigned RooterIdx) -> const MemRegion * {
    if (RooterIdx >= Call.getNumArgs())
      return nullptr;
    if (RooterIdx >= FD->getNumParams() ||
        !declHasAnnotation(FD->getParamDecl(RooterIdx),
                           "julia_require_rooted_slot"))
      return nullptr;
    const Expr *ArgExpr = Call.getArgExpr(RooterIdx);
    const Expr *ValueExpr = ignoreOuterPointerCasts(ArgExpr);
    const MemRegion *Region = getRootingRegionForExpr(ValueExpr, State, C);
    if (!Region)
      Region = getRegionForPointerValue(Call.getArgSVal(RooterIdx), C);
    if (!Region)
      return nullptr;
    Region = Region->StripCasts();
    if (isRootingRegion(State, Region) || State->get<GCRootMap>(Region))
      return Region;
    return nullptr;
  };

  auto getRootedObjects = [&](unsigned RootedIdx) {
    if (RootedIdx >= Call.getNumArgs())
      return emptyObjectSet(State);
    const Expr *RootedExpr = Call.getArgExpr(RootedIdx);
    RootedExpr = ignoreOuterPointerCasts(RootedExpr);
    if (const MemRegion *StorageRegion =
            getStorageRegionForExpr(RootedExpr, State, C)) {
      GCObjectSet Objects = getObjectsForRegionOrParents(State, StorageRegion);
      if (!Objects.isEmpty())
        return Objects;
    }
    SVal RootedSVal = Call.getArgSVal(RootedIdx);
    GCObjectSet Objects = getObjectsForSVal(State, RootedSVal);
    if (Objects.isEmpty()) {
      if (SymbolRef RootedSymbol = RootedSVal.getAsSymbol(true)) {
        if (GCObject Object = ensureObjectForSymbol(RootedSymbol, State, C))
          Objects = singletonObjectSet(State, Object);
      }
    }
    return Objects;
  };

  bool Changed = false;

  auto rooterUsesIndex = [&](unsigned RooterIdx,
                             std::optional<NonLoc> Index) {
    if (!Index)
      return false;
    return RooterIdx >= FD->getNumParams() ||
           !isGenericMemoryRefType(FD->getParamDecl(RooterIdx)->getType());
  };

  auto addRootingEdges = [&](unsigned RooterIdx,
                             std::optional<NonLoc> Index,
                             GCObjectSet RootedObjects) {
    GCObjectSet ParentObjects = getRooterObjects(RooterIdx);
    if (!ParentObjects.isEmpty()) {
      if (rooterUsesIndex(RooterIdx, Index)) {
        for (GCObject ParentObject : ParentObjects) {
          const MemRegion *ChildRegion =
              getIndexedObjectRegion(ParentObject, *Index, C);
          State = addObjectRegionEdge(State, ParentObject, ChildRegion);
          State = bindRegionToObjects(State, ChildRegion, RootedObjects, C);
        }
        return true;
      }
      bool Added = false;
      for (GCObject ParentObject : ParentObjects) {
        State = addObjectOwnershipEdge(State, ParentObject, RootedObjects);
        Added = true;
      }
      return Added;
    }
    if (const MemRegion *RootingRegion = getRootingRegion(RooterIdx)) {
      State = bindRegionToObjects(State, RootingRegion, RootedObjects, C);
      return true;
    }
    return false;
  };

  struct RootedByArgEntry {
    unsigned Rooter;
    std::optional<NonLoc> Index;
    GCObjectSet Objects;
  };
  llvm::SmallVector<RootedByArgEntry, 4> RootedByRooter;
  for (unsigned i = 0; i < FD->getNumParams(); ++i) {
    if (i >= Call.getNumArgs())
      break;
    // Plain JL_ROOTED_BY_ARG models ownership by the rooter object. Indexed
    // replacement precision is opt-in so unrelated integer arguments are not
    // mistaken for storage indices.
    std::optional<std::pair<unsigned, unsigned>> IndexedRooter =
        declHasIndexedPairAnnotation(FD->getParamDecl(i),
                                     "julia_rooted_by_arg_indexed:");
    std::optional<unsigned> Rooter;
    if (IndexedRooter)
      Rooter = IndexedRooter->first;
    else
      Rooter =
          declHasIndexedAnnotation(FD->getParamDecl(i), "julia_rooted_by_arg:");
    if (!Rooter)
      continue;
    GCObjectSet RootedObjects = getRootedObjects(i);
    std::optional<NonLoc> Index;
    if (IndexedRooter && IndexedRooter->second < Call.getNumArgs()) {
      SVal Converted =
          C.getSValBuilder().convertToArrayIndex(
              Call.getArgSVal(IndexedRooter->second));
      if (auto ConvertedIndex = Converted.getAs<NonLoc>())
        Index = *ConvertedIndex;
    }
    bool Found = false;
    for (auto &Entry : RootedByRooter) {
      if (Entry.Rooter != *Rooter || Entry.Index != Index)
        continue;
      for (GCObject Object : RootedObjects)
        Entry.Objects = State->get_context<GCObjectSet>().add(Entry.Objects,
                                                             Object);
      Found = true;
      break;
    }
    if (!Found)
      RootedByRooter.push_back({*Rooter, Index, RootedObjects});
  }
  for (const auto &Entry : RootedByRooter)
    Changed |= addRootingEdges(Entry.Rooter, Entry.Index, Entry.Objects);

  for (unsigned i = 0; i < FD->getNumParams(); ++i) {
    if (i >= Call.getNumArgs())
      break;
    if (declHasAnnotation(FD->getParamDecl(i), "julia_require_rooted_slot")) {
      const MemRegion *RootRegion = getRegionForPointerValue(Call.getArgSVal(i), C);
      if (!RootRegion) {
        const Expr *ArgExpr = Call.getArgExpr(i);
        RootRegion = getStorageRegionForExpr(ignoreOuterPointerCasts(ArgExpr),
                                             State, C);
      }
      if (!RootRegion)
        continue;
      RootRegion = RootRegion->StripCasts();
      QualType ValueType;
      QualType ParamType = FD->getParamDecl(i)->getType();
      if (ParamType->isPointerType())
        ValueType = ParamType->getPointeeType();
      State = State->set<GCRootMap>(RootRegion, -1);
      State = addPermanentRoot(State, RootRegion);
      State = State->add<GCConservativeRootRegions>(RootRegion);
      State = bindRootRegionToCurrentValue(State, RootRegion, ValueType, C);
      Changed = true;
    }
    std::optional<unsigned> Rooter = declHasIndexedAnnotation(
        FD->getParamDecl(i), "julia_out_rooted_by_arg:");
    if (!Rooter)
      continue;
    SVal OutPointer = Call.getArgSVal(i);
    const MemRegion *OutRegion = getRegionForPointerValue(OutPointer, C);
    if (!OutRegion) {
      const Expr *ArgExpr = Call.getArgExpr(i);
      OutRegion = getStorageRegionForExpr(ignoreOuterPointerCasts(ArgExpr),
                                          State, C);
    }
    if (!OutRegion)
      continue;
    QualType OutType;
    QualType ParamType = FD->getParamDecl(i)->getType();
    if (ParamType->isPointerType())
      OutType = ParamType->getPointeeType();
    State = bindRootRegionToCurrentValue(
        State, OutRegion, OutType, C,
        RootValueBinding::ConjurePossibleOutValue);
    GCObjectSet OutObjects = getObjectsForRegion(State, OutRegion);
    SVal OutValue = State->getSVal(OutRegion);
    if (OutObjects.isEmpty()) {
      if (SymbolRef OutSym = OutValue.getAsSymbol(true)) {
        if (GCObject Object = ensureObjectForSymbol(OutSym, State, C))
          OutObjects = singletonObjectSet(State, Object);
      }
    }
    if (SymbolRef OutSym = OutValue.getAsSymbol(true))
      State = setObjectsForSymbol(State, OutSym, OutObjects, C);

    GCObjectSet ParentObjects = getRooterObjects(*Rooter);
    if (!ParentObjects.isEmpty()) {
      for (GCObject ParentObject : ParentObjects) {
        State = addObjectOwnershipEdge(State, ParentObject, OutObjects);
      }
      Changed = true;
    } else if (const MemRegion *RootingRegion = getRootingRegion(*Rooter)) {
      State = bindRegionToObjects(State, RootingRegion, OutObjects, C);
      Changed = true;
    } else {
      Changed = true;
    }
  }

  return Changed;
}

bool GCChecker::processRootPropagatingRegionResult(
    const CallEvent &Call, CheckerContext &C, ProgramStateRef &State) const {
  QualType QT = Call.getResultType();
  bool IsPointerCarrier = QT->isPointerType() && !isGCObjectType(QT);
  bool IsTrackedValueCarrier = !QT->isPointerType() && isGCTrackedType(QT);
  if (!IsPointerCarrier && !IsTrackedValueCarrier)
    return false;
  if (!Call.getOriginExpr())
    return false;
  const FunctionDecl *FD =
      Call.getDecl() ? Call.getDecl()->getAsFunction() : nullptr;
  if (!FD)
    return false;

  GCObjectSet ParentObjects = emptyObjectSet(State);
  for (unsigned i = 0; i < FD->getNumParams(); ++i) {
    if (!declHasAnnotation(FD->getParamDecl(i), "julia_propagates_root"))
      continue;
    SVal Arg = Call.getArgSVal(i);
    ParentObjects = getObjectsForRootPropagatingArgument(Call, i, State, C);
    if (ParentObjects.isEmpty()) {
      if (const MemRegion *ArgRegion = Arg.getAsRegion())
        ParentObjects = getObjectsForRegionOrParents(State, ArgRegion);
    }
    break;
  }
  if (ParentObjects.isEmpty())
    return false;

  SVal Result = Call.getReturnValue();
  const MemRegion *ResultRegion = Result.getAsRegion();
  if (!ResultRegion && IsTrackedValueCarrier)
    ResultRegion = getStorageRegionForExpr(Call.getOriginExpr(), State, C);
  const MemRegion *BindingRegion = nullptr;
  if (IsTrackedValueCarrier)
    BindingRegion =
        getBindingRegionForExprResult(Call.getOriginExpr(), State, C);
  if (!ResultRegion)
    ResultRegion = BindingRegion;
  if (!ResultRegion && IsPointerCarrier) {
    Result = C.getSValBuilder().conjureSymbolVal(
        C.getCFGElementRef(), C.getLocationContext(), QT, C.blockCount());
    State = State->BindExpr(Call.getOriginExpr(), C.getLocationContext(),
                            Result);
    ResultRegion = Result.getAsRegion();
  }
  if (!ResultRegion)
    return false;
  State = addObjectRegionEdges(State, ParentObjects, ResultRegion);
  State = bindRegionToObjects(State, ResultRegion, ParentObjects, C);
  if (BindingRegion && BindingRegion->StripCasts() != ResultRegion->StripCasts()) {
    State = addObjectRegionEdges(State, ParentObjects, BindingRegion);
    State = bindRegionToObjects(State, BindingRegion, ParentObjects, C);
  }
  if (SymbolRef ResultSym = Result.getAsSymbol(true))
    State = setObjectsForSymbol(State, ResultSym, ParentObjects, C);
  return true;
}

bool GCChecker::processAllocationOfResult(const CallEvent &Call,
                                          CheckerContext &C,
                                          ProgramStateRef &State) const {
  QualType QT = Call.getResultType();
  if (!isGCObjectType(QT))
    return false;
  if (!Call.getOriginExpr()) {
    return false;
  }
  const FunctionDecl *FD =
      Call.getDecl() ? Call.getDecl()->getAsFunction() : nullptr;
  StringRef FDName =
      FD && FD->getDeclName().isIdentifier() ? FD->getName() : "";
  if (FDName == "alloca" || FDName == "__builtin_alloca" ||
      isAllocaExpr(Call.getOriginExpr()))
    return false;
  SVal ResultValue = Call.getReturnValue();
  SymbolRef Sym = ResultValue.getAsSymbol();
  if (!Sym) {
    ResultValue = C.getSValBuilder().conjureSymbolVal(
        C.getCFGElementRef(), C.getLocationContext(), QT, C.blockCount());
    Sym = ResultValue.getAsSymbol();
  }
  if (!ResultValue.isUnknown())
    State = State->BindExpr(Call.getOriginExpr(), C.getLocationContext(),
                            ResultValue);
  LivenessState NewVState = LivenessState::getAllocated();
  GCObjectSet RootPropagatingObjects = emptyObjectSet(State);
  std::optional<unsigned> RootPropagatingParam;
  std::optional<NonLoc> RootPropagatingIndex;
  bool ResultIsPermanentRoot = isGloballyRootedType(QT);
  if (!ResultIsPermanentRoot) {
    std::optional<LivenessState> ValS = getStateForSymbol(State, Sym);
    NewVState = ValS ? *ValS : LivenessState::getAllocated();
    if (FD) {
      if (declHasAnnotation(FD, "julia_globally_rooted")) {
        ResultIsPermanentRoot = true;
      } else {
        // Special case for jl_box_ functions which have value-dependent
        // global roots.
        StringRef FDName =
            FD->getDeclName().isIdentifier() ? FD->getName() : "";
        if (FDName.starts_with("jl_box_") || FDName.starts_with("ijl_box_")) {
          SVal Arg = Call.getArgSVal(0);
          if (auto CI = Arg.getAs<nonloc::ConcreteInt>()) {
            const llvm::APSInt &Value = CI->getValue();
            bool GloballyRooted = false;
            const int64_t NBOX_C = 1024;
            if (FDName.starts_with("jl_box_u") || FDName.starts_with("ijl_box_u")) {
              if (Value < NBOX_C) {
                GloballyRooted = true;
              }
            } else {
              if (-NBOX_C / 2 < Value && Value < (NBOX_C - NBOX_C / 2)) {
                GloballyRooted = true;
              }
            }
            if (GloballyRooted) {
              ResultIsPermanentRoot = true;
            }
          }
        } else if (auto IndexedRooter = declHasIndexedPairAnnotation(
                       FD, "julia_propagates_root_indexed:")) {
          if (IndexedRooter->first < Call.getNumArgs()) {
            SVal Test = Call.getArgSVal(IndexedRooter->first);
            GCObjectSet TestObjects = getObjectsForRootPropagatingArgument(
                Call, IndexedRooter->first, State, C);
            if (TestObjects.isEmpty()) {
              const MemRegion *Region = Test.getAsRegion();
              TestObjects = getObjectsForRegionOrParents(State, Region);
            }
            if (!TestObjects.isEmpty()) {
              RootPropagatingObjects = TestObjects;
              RootPropagatingParam = IndexedRooter->first;
              NewVState = aggregateObjectState(State, TestObjects);
              if (IndexedRooter->second < Call.getNumArgs()) {
                SVal Converted =
                    C.getSValBuilder().convertToArrayIndex(
                        Call.getArgSVal(IndexedRooter->second));
                if (auto ConvertedIndex = Converted.getAs<NonLoc>()) {
                  const Expr *IndexExpr =
                      Call.getArgExpr(IndexedRooter->second);
                  IndexExpr =
                      IndexExpr ? IndexExpr->IgnoreParenImpCasts() : nullptr;
                  // Treat only literal indexed reads as replaceable object
                  // slots. Loop variables can be concrete on one analyzer
                  // path, while nearby expressions such as i and i+1 may
                  // still collapse to the same symbolic region. Keep those as
                  // opaque ownership edges so the over-approximation errs
                  // toward retaining roots.
                  if (isa_and_nonnull<IntegerLiteral>(IndexExpr) &&
                      ConvertedIndex->getAs<nonloc::ConcreteInt>())
                    RootPropagatingIndex = *ConvertedIndex;
                }
              }
            }
          }
        } else {
          for (unsigned i = 0; i < FD->getNumParams(); ++i) {
            if (declHasAnnotation(FD->getParamDecl(i),
                                  "julia_propagates_root")) {
              SVal Test = Call.getArgSVal(i);
              GCObjectSet TestObjects =
                  getObjectsForRootPropagatingArgument(Call, i, State, C);
              if (!TestObjects.isEmpty()) {
                RootPropagatingObjects = TestObjects;
                RootPropagatingParam = i;
                NewVState = aggregateObjectState(State, TestObjects);
                break;
              }
              // Walk backwards to find the tracked region that owns this value.
              const MemRegion *Region = Test.getAsRegion();
              TestObjects = getObjectsForRegionOrParents(State, Region);
              if (!TestObjects.isEmpty()) {
                RootPropagatingObjects = TestObjects;
                RootPropagatingParam = i;
                NewVState = aggregateObjectState(State, TestObjects);
              }
              break;
            }
          }
        }
      }
    }
  }
  GCObject Object = ensureObjectForSymbol(Sym, State, C, NewVState);
  if (Object) {
    if (ResultIsPermanentRoot)
      State = addPermanentRoot(State, getObjectRegion(Object, C));
    GCObjectSet ResultObjects = singletonObjectSet(State, Object);
    if (!RootPropagatingObjects.isEmpty()) {
      State = State->set<GCObjectStateMap>(Object, NewVState);
      if (!RootPropagatingIndex)
        State = addObjectOwnershipEdges(State, RootPropagatingObjects,
                                        ResultObjects);
      auto resultMayAliasExplicitRootedArgument = [&]() {
        if (!RootPropagatingParam)
          return false;
        // Setter-style helpers such as jl_svecset return the stored argument.
        // The rooted-by-arg edge already models that store, and without a
        // return-alias annotation a fresh call result would overwrite it.
        for (unsigned i = 0; i < FD->getNumParams(); ++i) {
          std::optional<unsigned> Rooter = declHasIndexedAnnotation(
              FD->getParamDecl(i), "julia_rooted_by_arg:");
          if (!Rooter) {
            if (auto IndexedRooter = declHasIndexedPairAnnotation(
                    FD->getParamDecl(i),
                    "julia_rooted_by_arg_indexed:"))
              Rooter = IndexedRooter->first;
          }
          if (!Rooter || *Rooter != *RootPropagatingParam)
            continue;
          return true;
        }
        return false;
      };
      if (RootPropagatingIndex && !resultMayAliasExplicitRootedArgument()) {
        for (GCObject ParentObject : RootPropagatingObjects) {
          const MemRegion *ChildRegion =
              getIndexedObjectRegion(ParentObject, *RootPropagatingIndex, C);
          State = addObjectRegionEdge(State, ParentObject, ChildRegion);
          State = bindRegionToObjects(State, ChildRegion, ResultObjects, C);
        }
      }
    }
    if (FD) {
      unsigned NumArgs = Call.getNumArgs();
      unsigned NumParams = FD->getNumParams();
      bool RootedVarargs = declHasAnnotation(FD, "julia_rooted_varargs");
      for (unsigned i = 0; i < NumArgs; ++i) {
        bool IsRootedArgument =
            i < NumParams &&
            declHasAnnotation(FD->getParamDecl(i),
                              "julia_rooted_by_return");
        if (!IsRootedArgument &&
            !(RootedVarargs && FD->isVariadic() && i >= NumParams))
          continue;
        SVal Arg = Call.getArgSVal(i);
        GCObjectSet ArgObjects = getObjectsForSVal(State, Arg);
        if (ArgObjects.isEmpty()) {
          if (SymbolRef ArgSym = Arg.getAsSymbol()) {
            if (GCObject ArgObject = ensureObjectForSymbol(ArgSym, State, C))
              ArgObjects = singletonObjectSet(State, ArgObject);
          }
        }
        if (ArgObjects.isEmpty())
          continue;
        const MemRegion *ChildRegion = getIndexedObjectRegion(Object, i, C);
        State = addObjectRegionEdge(State, Object, ChildRegion);
        State = bindRegionToObjects(State, ChildRegion, ArgObjects, C);
      }
    }
  }
  return true;
}

void GCChecker::checkPostCall(const CallEvent &Call, CheckerContext &C) const {
  ProgramStateRef State = C.getState();
  bool didChange = processArgumentRooting(Call, C, State);
  if (!C.wasInlined)
    didChange |= processPotentialSafepoint(Call, C, State);
  didChange |= processRootPropagatingRegionResult(Call, C, State);
  didChange |= processAllocationOfResult(Call, C, State);
  if (didChange)
    C.addTransition(State);
}

// Implicitly root values that were casted to globally rooted values
void GCChecker::checkPostStmt(const CStyleCastExpr *CE,
                              CheckerContext &C) const {
  if (!isGloballyRootedType(CE->getTypeAsWritten()))
    return;
  SymbolRef Sym = C.getSVal(CE).getAsSymbol();
  if (!Sym)
    return;
  ProgramStateRef State = promiseRootedSVal(C.getState(), C.getSVal(CE), C);
  C.addTransition(State);
}

SymbolRef GCChecker::getSymbolForResult(const Expr *Result,
                                        bool ShouldConjure,
                                        ProgramStateRef &State,
                                        CheckerContext &C) const {
  QualType QT = getAtomicValueType(Result->getType());
  if (!QT->isPointerType() || QT->getPointeeType()->isVoidType())
    return nullptr;
  auto ValLoc = State->getSVal(Result, C.getLocationContext()).getAs<Loc>();
  if (!ValLoc) {
    return nullptr;
  }
  SVal Loaded = State->getSVal(*ValLoc);
  if (Loaded.isUnknown() || !Loaded.getAsSymbol()) {
    if (ShouldConjure || GCChecker::isGCTracked(Result)) {
      Loaded = C.getSValBuilder().conjureSymbolVal(
          nullptr, C.getCFGElementRef(), C.getLocationContext(),
          QT, C.blockCount());
      State = State->bindLoc(*ValLoc, Loaded, C.getLocationContext());
      // State = State->BindExpr(Result, C.getLocationContext(),
      // State->getSVal(*ValLoc));
    }
  }
  return Loaded.getAsSymbol();
}

void GCChecker::checkDerivingExpr(const Expr *Result, const Expr *Parent,
                                  CheckerContext &C) const {
  Parent = Parent->IgnoreParenCasts();
  if (auto UO = dyn_cast<UnaryOperator>(Parent)) {
    if (UO->getOpcode() == UO_AddrOf) {
      Parent = UO->getSubExpr()->IgnoreParenCasts();
    }
  }
  bool ResultTracked = true;
  ProgramStateRef State = C.getState();
  if (isGloballyRootedType(Result->getType())) {
    SymbolRef NewSym = getSymbolForResult(Result, false, State, C);
    if (!NewSym) {
      return;
    }
    std::optional<LivenessState> NewValS = getStateForSymbol(State, NewSym);
    GCObjectSet NewObjects = getObjectsForSymbol(State, NewSym);
    if (NewValS && objectsAreReachable(State, NewObjects)) {
      return;
    }
    State = promiseRootedSVal(State, State->getSVal(Result, C.getLocationContext()), C);
    C.addTransition(State);
    return;
  }
  if (!isGCTracked(Result)) {
    // TODO: We may want to refine this. This is to track pointers through the
    // array list in jl_module_t.
    bool ParentIsModule = isJuliaType(
        [](StringRef Name) { return Name.ends_with("jl_module_t"); },
        Parent->getType());
    bool ResultIsArrayList = isJuliaType(
        [](StringRef Name) { return Name.ends_with("arraylist_t"); },
        Result->getType());
    if (!(ParentIsModule && ResultIsArrayList) && isGCTracked(Parent)) {
      ResultTracked = false;
    }
  }
  // This is the pointer
  auto ResultVal = C.getSVal(Result);
  if (ResultVal.isUnknown()) {
    QualType ResultType = getAtomicValueType(Result->getType());
    if (!ResultType->isPointerType()) {
      return;
    }
    ResultVal = C.getSValBuilder().conjureSymbolVal(
        C.getCFGElementRef(), C.getLocationContext(), ResultType,
        C.blockCount());
    State = State->BindExpr(Result, C.getLocationContext(), ResultVal);
  }
  auto ValLoc = ResultVal.getAs<Loc>();
  if (!ValLoc)
    return;
  const MemRegion *ResultRegion = ResultVal.getAsRegion();
  const MemRegion *ResultStorageRegion =
      getStorageRegionForExpr(Result, State, C);
  SVal ParentVal = C.getSVal(Parent);
  SymbolRef OldSym = ParentVal.getAsSymbol(true);
  const MemRegion *ParentRegion = ParentVal.getAsRegion();
  const MemRegion *ParentStorageRegion =
      getStorageRegionForExpr(Parent, State, C);
  std::optional<LivenessState> OldValS = getStateForSymbol(State, OldSym);
  GCObjectSet ParentObjects =
      getObjectsForExprValue(Parent, ParentVal, State, C);
  GCObjectSet Reachable = computeReachableObjects(State);
  if (!ParentObjects.isEmpty() &&
      objectsMayBeFreed(State, ParentObjects, Reachable)) {
    report_value_error(C, *ParentObjects.begin(),
                       "Creating derivative of value that may have been GCed");
    return;
  }
  if (!OldValS && !ParentObjects.isEmpty()) {
    OldValS = aggregateObjectState(State, ParentObjects);
  }
  SymbolRef NewSym = getSymbolForResult(Result, OldValS.has_value(), State, C);
  if (!NewSym) {
    return;
  }
  // NewSym might already have a better root
  std::optional<LivenessState> NewValS = getStateForSymbol(State, NewSym);
  GCObjectSet NewObjects = getObjectsForSymbol(State, NewSym);
  if (NewObjects.isEmpty() && ResultStorageRegion) {
    GCObjectSet RegionObjects = getObjectsForRegion(State, ResultStorageRegion);
    if (!RegionObjects.isEmpty()) {
      NewObjects = RegionObjects;
      State = setObjectsForSymbol(State, NewSym, NewObjects, C);
    }
  }
  if (NewObjects.isEmpty() && ResultRegion) {
    GCObjectSet RegionObjects = getObjectsForRegion(State, ResultRegion);
    if (!RegionObjects.isEmpty()) {
      NewObjects = RegionObjects;
      State = setObjectsForSymbol(State, NewSym, NewObjects, C);
    }
  }
  if (NewObjects.isEmpty() && ResultStorageRegion &&
      !ResultStorageRegion->getAs<SymbolicRegion>() &&
      isRootingRegion(State, ResultStorageRegion)) {
    GCObjectSet RegionObjects = getObjectsForRegion(State, ResultStorageRegion);
    if (!RegionObjects.isEmpty()) {
      NewObjects = RegionObjects;
      State = setObjectsForSymbol(State, NewSym, NewObjects, C);
    }
  }
  if (NewObjects.isEmpty() && !ParentObjects.isEmpty() &&
      Result->getType()->isPointerType() && !isGCObjectType(Result->getType())) {
    // Non-object carriers such as jl_svec_data(obj) are not Julia objects
    // themselves, but accesses through them are still owned by the parent
    // object. Keep that ownership edge so a freed parent is still diagnosed
    // when the carrier is dereferenced later.
    State = setObjectsForSymbol(State, NewSym, ParentObjects, C);
    NewObjects = ParentObjects;
  }
  if (NewObjects.isEmpty()) {
    if (GCObject NewObject =
            ensureObjectForSymbol(NewSym, State, C,
                                  OldValS && OldValS->isPotentiallyFreed()
                                      ? LivenessState::getFreed()
                                      : LivenessState::getAllocated())) {
      NewObjects = singletonObjectSet(State, NewObject);
    }
  }
  if (ResultStorageRegion && !ParentObjects.isEmpty()) {
    State = addObjectRegionEdges(State, ParentObjects, ResultStorageRegion);
    if (!NewObjects.isEmpty())
      State = bindRegionToObjects(State, ResultStorageRegion, NewObjects, C);
  }
  if (ResultRegion && !ParentObjects.isEmpty()) {
    State = addObjectRegionEdges(State, ParentObjects, ResultRegion);
    if (!NewObjects.isEmpty())
      State = bindRegionToObjects(State, ResultRegion, NewObjects, C);
  }
  if (ParentRegion || ParentStorageRegion) {
    const MemRegion *DiagnosticRegion = ParentRegion ? ParentRegion
                                                     : ParentStorageRegion;
    const VarRegion *VR = DiagnosticRegion->getAs<VarRegion>();
    bool inheritedState = false;
    bool inheritedRoot = false;
    LivenessState Updated = LivenessState::getAllocated();
    if (VR && isa<ParmVarDecl>(VR->getDecl())) {
      // This works around us not being able to track symbols for struct/union
      // parameters very well.
      const auto *FD =
          dyn_cast<FunctionDecl>(C.getLocationContext()->getDecl());
      if (FD) {
        inheritedState = true;
        bool isFunctionSafepoint = !isFDAnnotatedNotSafepoint(FD, getSM(C));
        const ParmVarDecl *PVD = cast<ParmVarDecl>(VR->getDecl());
        Updated = LivenessState::getForArgument(FD, PVD, isFunctionSafepoint);
        inheritedRoot =
            isFunctionSafepoint && !declHasAnnotation(PVD, "julia_maybe_unrooted");
      }
    } else {
      VR = Helpers::walk_back_to_global_VR(DiagnosticRegion);
      if (VR) {
        if (VR && rootRegionIfGlobal(VR, State, C)) {
          inheritedState = true;
          inheritedRoot = true;
        }
      }
    }
    if (inheritedState && ResultTracked) {
      State = setObjectsForSymbol(State, NewSym, NewObjects, C);
      if (inheritedRoot)
        for (GCObject Object : NewObjects)
          State = addPermanentRoot(State, getObjectRegion(Object, C));
      for (GCObject Object : NewObjects)
        State = State->set<GCObjectStateMap>(Object, Updated);
      C.addTransition(State);
      return;
    }
  }
  if (NewValS &&
      objectsAreReachable(NewObjects, computeReachableObjects(State))) {
    return;
  }
  if (!OldValS) {
    // This way we'll get better diagnostics
    if (isGCTracked(Result)) {
      for (GCObject Object : NewObjects)
        State = State->set<GCObjectStateMap>(Object, LivenessState::getUntracked());
      C.addTransition(State);
    }
    return;
  }
  if (OldValS->isPotentiallyFreed()) {
    report_value_error(C, OldSym,
                       "Creating derivative of value that may have been GCed");
  } else if (ResultTracked) {
    if (OldValS->isUntracked()) {
      for (GCObject Object : NewObjects)
        State = State->set<GCObjectStateMap>(Object, *OldValS);
    }
    State = setObjectsForSymbol(State, NewSym, NewObjects, C);
    for (GCObject Object : NewObjects)
      State = State->set<GCObjectStateMap>(Object, *OldValS);
    C.addTransition(State);
    return;
  }
}

// Propagate rootedness through subscript
void GCChecker::checkPostStmt(const ArraySubscriptExpr *ASE,
                              CheckerContext &C) const {
  if (isAssignmentLHS(ASE, C))
    return;
  checkDerivingExpr(ASE, ASE->getLHS(), C);
}

void GCChecker::checkPostStmt(const MemberExpr *ME, CheckerContext &C) const {
  if (isAssignmentLHS(ME, C))
    return;
  clang::Expr *Base = ME->getBase();
  if (!isGCTracked(ME)) {
    // Keep non-GC carrier fields that are known to expose GC-owned storage.
    bool ParentIsModule = isJuliaType(
        [](StringRef Name) { return Name.ends_with("jl_module_t"); },
        Base->getType());
    bool ResultIsArrayList = isJuliaType(
        [](StringRef Name) { return Name.ends_with("arraylist_t"); },
        ME->getType());
    if (!(ParentIsModule && ResultIsArrayList))
      return;
  }
  checkDerivingExpr(ME, Base, C);
}

void GCChecker::checkPostStmt(const UnaryOperator *UO,
                              CheckerContext &C) const {
  if (isAssignmentLHS(UO, C))
    return;
  if (UO->getOpcode() == UO_Deref) {
    checkDerivingExpr(UO, UO->getSubExpr(), C);
  }
}

void GCChecker::checkPreCall(const CallEvent &Call, CheckerContext &C) const {
  if (!gcEnabledHere(C))
    return;
  unsigned NumArgs = Call.getNumArgs();
  ProgramStateRef State = C.getState();
  bool DidChange = false;
  std::optional<GCObjectSet> Reachable;
  auto getReachable = [&]() {
    if (!Reachable)
      Reachable = computeReachableObjects(State);
    return *Reachable;
  };
  bool isCalleeSafepoint = isSafepoint(Call, C);
  auto *Decl = Call.getDecl();
  const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
  StringRef FDName =
      FD && FD->getDeclName().isIdentifier() ? FD->getName() : "";
  if (isMutexUnlock(FDName) || (FD && declHasAnnotation(FD, "julia_notsafepoint_leave"))) {
    const auto *LCtx = C.getLocationContext();
    const auto *FD = dyn_cast<FunctionDecl>(LCtx->getDecl());
    if (State->get<SafepointDisabledAt>() == getStackFrameHeight(C.getStackFrame()) &&
        !isFDAnnotatedNotSafepoint(FD, getSM(C))) {
      State = State->set<SafepointDisabledAt>((unsigned)-1);
      C.addTransition(State);
    }
  }
  if (!safepointEnabledHere(State) && isCalleeSafepoint) {
    // Suppress this warning if the function is noreturn.
    // We could separate out "not safepoint, except for noreturn functions",
    // but that seems like a lot of effort with little benefit.
    if (!FD || !FD->isNoReturn()) {
      report_error(
          [&](PathSensitiveBugReport *Report) {
            if (FD)
              Report->addNote(
                  "Tried to call method defined here",
                  PathDiagnosticLocation::create(FD, C.getSourceManager()));
            Report->addVisitor(make_unique<SafepointBugVisitor>());
          },
          C, ("Calling potential safepoint as " +
              Call.getKindAsString() + " from function annotated JL_NOTSAFEPOINT").str());
      return;
    }
  }
  if (FD && FD->getDeclName().isIdentifier() &&
      FD->getName() == "JL_GC_PROMISE_ROOTED")
    return;
  if (FDName.starts_with("JL_GC_PUSH") || FDName == "_JL_GC_PUSHARGS" ||
      FDName == "JL_GC_POP")
    return;
  for (unsigned idx = 0; idx < NumArgs; ++idx) {
    SVal Arg = Call.getArgSVal(idx);
    if (isAllocaDerivedSVal(State, Arg))
      continue;
    SymbolRef Sym = Arg.getAsSymbol();
    if (!Sym)
      Sym = Arg.getAsSymbol(true);
    // Hack to work around passing unions/structs by value.
    if (auto LCV = Arg.getAs<nonloc::LazyCompoundVal>()) {
      const MemRegion *R = LCV->getRegion();
      if (R) {
        if (const SubRegion *SR = R->getAs<SubRegion>()) {
          if (const SymbolicRegion *SSR =
                  SR->getSuperRegion()->getAs<SymbolicRegion>()) {
            Sym = SSR->getSymbol();
          }
        }
      }
    }
    const Expr *ArgExpr = Call.getArgExpr(idx);
    if (Sym) {
      bool RootedGlobal = rootRegionIfGlobal(Sym->getOriginRegion(), State, C);
      DidChange |= RootedGlobal;
      if (RootedGlobal)
        Reachable.reset();
    }
    GCObjectSet ArgObjects = getObjectsForSymbol(State, Sym);
    if (ArgObjects.isEmpty())
      ArgObjects = getObjectsForExprValue(ArgExpr, Arg, State, C);
    SymbolRef ReportSym =
        Sym ? Sym : (ArgObjects.isEmpty() ? nullptr : *ArgObjects.begin());
    if (!ReportSym)
      continue;
    std::optional<LivenessState> ValState = getStateForSymbol(State, Sym);
    if (!ValState && !ArgObjects.isEmpty())
      ValState = aggregateObjectState(State, ArgObjects);
    if (!ValState || ValState->isUntracked())
      continue;
    SourceRange range;
    if (ArgExpr) {
      range = ArgExpr->getSourceRange();
      if (!isGCObjectType(ArgExpr->IgnoreParenCasts()->getType()))
        continue;
    }
    GCObjectSet CurrentReachable = getReachable();
    if (objectsAreReachable(ArgObjects, CurrentReachable))
      continue;
    bool ReportedFreedArgument = false;
    if (ValState->isPotentiallyFreed()) {
      report_value_error(C, ReportSym, "Argument value may have been GCed",
                         range);
      ReportedFreedArgument = true;
    }
    if (!ReportedFreedArgument &&
        objectsMayBeFreed(State, ArgObjects, CurrentReachable)) {
      report_value_error(C, *ArgObjects.begin(),
                         "Argument value may have been GCed", range);
      ReportedFreedArgument = true;
    }
    bool MaybeUnrooted = false;
    if (FD) {
      if (idx < FD->getNumParams()) {
        MaybeUnrooted =
            declHasAnnotation(FD->getParamDecl(idx), "julia_maybe_unrooted");
      }
    }
    if (!MaybeUnrooted && isCalleeSafepoint) {
      report_value_error(
          C, ReportSym,
          "Passing non-rooted value as argument to function that may GC",
          range);
    }
  }
  if (DidChange)
    C.addTransition(State);
}

bool GCChecker::evalCall(const CallEvent &Call, CheckerContext &C) const {
  // These checks should have no effect on the surrounding environment
  // (globals should not be invalidated, etc), hence the use of evalCall.
  const CallExpr *CE = dyn_cast<CallExpr>(Call.getOriginExpr());
  unsigned CurrentDepth = C.getState()->get<GCDepth>();
  auto name = CE ? C.getCalleeName(CE) : "";
  if (name == "JL_GC_POP") {
    if (CurrentDepth == 0) {
      report_error(C, "JL_GC_POP without corresponding push");
      return true;
    }
    CurrentDepth -= 1;
    // Go through all roots, see which ones are no longer with us.
    // Then go through the values and unroot those for which those were our
    // roots.
    ProgramStateRef State = C.getState()->set<GCDepth>(CurrentDepth);
    State = removeFrameRoots(State, CurrentDepth);
    GCRootMapTy AMap = State->get<GCRootMap>();
    for (auto I = AMap.begin(), E = AMap.end(); I != E; ++I) {
      if (I.getData() == (int)CurrentDepth)
        State = State->remove<GCRootMap>(I.getKey());
    }
    C.addTransition(State);
    return true;
  } else if (name == "JL_GC_PUSH1" || name == "JL_GC_PUSH2" ||
             name == "JL_GC_PUSH3" || name == "JL_GC_PUSH4" ||
             name == "JL_GC_PUSH5" || name == "JL_GC_PUSH6" ||
             name == "JL_GC_PUSH7" || name == "JL_GC_PUSH8" ||
             name == "JL_GC_PUSH9") {
    ProgramStateRef State = C.getState();
    // Transform slots to roots, transform values to rooted
    unsigned NumArgs = CE->getNumArgs();
    for (unsigned i = 0; i < NumArgs; ++i) {
      SVal V = C.getSVal(CE->getArg(i));
      auto MRV = V.getAs<loc::MemRegionVal>();
      if (!MRV) {
        report_error(C, "JL_GC_PUSH with something other than a local variable");
        return true;
      }
      const MemRegion *Region = MRV->getRegion();
      State = State->set<GCRootMap>(Region, (int)CurrentDepth);
      State = addFrameRoot(State, CurrentDepth, Region, C);
      // Now for the value
      SVal Value = State->getSVal(Region);
      SymbolRef Sym = Value.getAsSymbol();
      if (!Sym)
        continue;
      ensureObjectForSymbol(Sym, State, C);
      State = bindRegionToSVal(State, Region, Value, C);
      std::optional<LivenessState> ValState = getStateForSymbol(State, Sym);
      if (!ValState)
        continue;
      if (ValState->isPotentiallyFreed()) {
        report_value_error(C, Sym,
                           "Trying to root value which may have been GCed");
        GCObjectSet Objects = getObjectsForSymbol(State, Sym);
        for (GCObject Object : Objects)
          State = State->set<GCObjectStateMap>(Object, LivenessState::getAllocated());
      }
    }
    CurrentDepth += 1;
    State = State->set<GCDepth>(CurrentDepth);
    C.addTransition(State);
    return true;
  } else if (name == "_JL_GC_PUSHARGS") {
    ProgramStateRef State = C.getState();
    SVal ArgArray = C.getSVal(CE->getArg(0));
    auto MRV = ArgArray.getAs<loc::MemRegionVal>();
    if (!MRV) {
      report_error(C, "JL_GC_PUSH with something other than an args array");
      return true;
    }
    const MemRegion *Region = MRV->getRegion()->StripCasts();
    State = addFrameRoot(State, CurrentDepth, Region, C);
    if (auto Count = C.getSVal(CE->getArg(1)).getAs<nonloc::ConcreteInt>()) {
      uint64_t N = Count->getValue()->getZExtValue();
      const auto *Base = dyn_cast<SubRegion>(Region);
      QualType ElementType = C.getASTContext().VoidPtrTy;
      QualType ArgType = CE->getArg(0)->getType();
      if (ArgType->isPointerType())
        ElementType = ArgType->getPointeeType();
      for (uint64_t I = 0; Base && I < N; ++I) {
        const MemRegion *Slot =
            C.getSValBuilder().getRegionManager().getElementRegion(
                ElementType, C.getSValBuilder().makeArrayIndex(I), Base,
                C.getASTContext());
        State = State->set<GCRootMap>(Slot, (int)CurrentDepth);
        State = addFrameRoot(State, CurrentDepth, Slot, C);
      }
    } else {
      State = State->set<GCRootMap>(Region, (int)CurrentDepth);
    }
    CurrentDepth += 1;
    State = State->set<GCDepth>(CurrentDepth);
    C.addTransition(State);
    return true;
  } else if (name == "JL_GC_PROMISE_ROOTED") {
    const Expr *ArgExpr = CE->getArg(0);
    const Expr *ValueExpr = ignoreOuterPointerCasts(ArgExpr);
    SVal Arg = C.getSVal(ArgExpr);
    if (Arg.isZeroConstant()) {
      C.addTransition(C.getState());
      return true;
    }
    ProgramStateRef State =
        promiseRootedSVal(C.getState(), Arg, C,
                          ValueExpr ? ValueExpr->getType() : ArgExpr->getType());
    SymbolRef Sym = Arg.getAsSymbol(true);
    GCObjectSet Objects = getObjectsForSVal(State, Arg);
    if (const MemRegion *ArgRegion =
            getStorageRegionForExpr(ValueExpr, State, C)) {
      State = addPermanentRoot(State, ArgRegion);
      if (!Objects.isEmpty())
        State = bindRegionToObjects(State, ArgRegion, Objects, C);
    }
    if (!Sym && getObjectsForSVal(State, Arg).isEmpty()) {
      report_error(C, "Can not understand this promise.");
      return true;
    }
    C.addTransition(State);
    return true;
  } else if (name == "jl_gc_push_arraylist") {
    ProgramStateRef State = C.getState();
    auto FinishPush = [&]() {
      State = State->set<GCDepth>(CurrentDepth + 1);
      C.addTransition(State);
      return true;
    };
    SVal ArrayList = C.getSVal(CE->getArg(1));
    // Try to find the items field
    FieldDecl *FD = NULL;
    RecordDecl *RD = dyn_cast_or_null<RecordDecl>(
        CE->getArg(1)->getType()->getPointeeType()->getAsTagDecl());
    if (RD) {
      for (FieldDecl *X : RD->fields()) {
        if (X->getName() == "items") {
          FD = X;
          break;
        }
      }
    }
    if (FD) {
      auto ItemsLocOpt = State->getLValue(FD, ArrayList).getAs<Loc>();
      if (!ItemsLocOpt)
        return FinishPush();
      Loc ItemsLoc = *ItemsLocOpt;
      SVal Items = State->getSVal(ItemsLoc);
      if (Items.isUnknown()) {
        Items = C.getSValBuilder().conjureSymbolVal(
            C.getCFGElementRef(), C.getLocationContext(), FD->getType(),
            C.blockCount());
        State = State->bindLoc(ItemsLoc, Items, C.getLocationContext());
      }
      assert(Items.getAsRegion());
      // The items list is now rooted
      if (const MemRegion *ItemsFieldRegion = ItemsLoc.getAsRegion()) {
        State = State->set<GCRootMap>(ItemsFieldRegion, (int)CurrentDepth);
        State = addFrameRoot(State, CurrentDepth, ItemsFieldRegion, C);
      }
      if (const MemRegion *ItemsRegion = Items.getAsRegion()) {
        State = State->set<GCRootMap>(ItemsRegion, (int)CurrentDepth);
        State = addFrameRoot(State, CurrentDepth, ItemsRegion, C);
      }
    }
    return FinishPush();
  } else if (name == "jl_ast_preserve") {
    // TODO: Maybe bind the rooting to the context. For now, the second
    //       argument gets unconditionally rooted
    SVal Preserved = C.getSVal(CE->getArg(1));
    ProgramStateRef State = promiseRootedSVal(C.getState(), Preserved, C);
    SymbolRef Sym = Preserved.getAsSymbol(true);
    if (!Sym)
      return true;
    C.addTransition(State);
    return true;
  } else if (name == "jl_gc_enable" || name == "ijl_gc_enable") {
    ProgramStateRef State = C.getState();
    // Check for a literal argument
    SVal Arg = C.getSVal(CE->getArg(0));
    auto CI = Arg.getAs<nonloc::ConcreteInt>();
    bool EnabledAfter = true;
    if (CI) {
      const llvm::APSInt &Val = CI->getValue();
      EnabledAfter = Val != 0;
    } else {
      cast<SymbolConjured>(Arg.getAsSymbol())->getStmt()->dump();
    }
    bool EnabledNow = gcEnabledHere(State);
    if (!EnabledAfter) {
      State = State->set<GCDisabledAt>((unsigned)-2);
    } else {
      State = State->set<GCDisabledAt>((unsigned)-1);
    }
    // GC State is explicitly modeled, so let's make sure
    // the execution matches our model
    SVal Result = C.getSValBuilder().makeTruthVal(EnabledNow, CE->getType());
    C.addTransition(State->BindExpr(CE, C.getLocationContext(), Result));
    return true;
  }
  {
    auto *Decl = Call.getDecl();
    const FunctionDecl *FD = Decl ? Decl->getAsFunction() : nullptr;
    if (isMutexLock(name) ||
        (FD && declHasAnnotation(FD, "julia_notsafepoint_enter"))) {
      ProgramStateRef State = C.getState();
      if (State->get<SafepointDisabledAt>() == (unsigned)-1) {
        C.addTransition(State->set<SafepointDisabledAt>(
            getStackFrameHeight(C.getStackFrame())));
        return true;
      }
    }
  }
  return false;
}

void GCChecker::checkBind(SVal LVal, SVal RVal, const clang::Stmt *S,
                          CheckerContext &C) const {
  auto State = C.getState();
  const MemRegion *R = LVal.getAsRegion();
  if (!R) {
    return;
  }
  SymbolRef Sym = RVal.getAsSymbol(true);
  GCObjectSet RValObjects = getObjectsForSVal(State, RVal);
  auto IsConjuredCallResult = [](SymbolRef Sym) {
    if (const auto *SCast = dyn_cast_or_null<SymbolCast>(Sym))
      Sym = SCast->getOperand();
    const auto *SC = dyn_cast_or_null<SymbolConjured>(Sym);
    return SC && isa_and_nonnull<CallExpr>(SC->getStmt());
  };
  if (Sym && RValObjects.isEmpty() && IsConjuredCallResult(Sym)) {
    if (GCObject Object = ensureObjectForSymbol(Sym, State, C))
      RValObjects = singletonObjectSet(State, Object);
  }
  if (Sym && rootRegionIfGlobal(Sym->getOriginRegion(), State, C)) {
    const MemRegion *OriginRegion = Sym->getOriginRegion();
    if (isRootingRegion(State, OriginRegion))
      ensureObjectForSymbol(Sym, State, C);
    RValObjects = getObjectsForSymbol(State, Sym);
  }
  bool IsRootingRegion = isRootingRegion(State, R);
  GCObjectSet LHSParentObjects = getTrackedParentObjects(State, R);
  GCObjectSet RHSParentObjects = emptyObjectSet(State);
  bool BoundDerivedObjects = false;
  if (Sym && Sym->getType()->isPointerType())
    RHSParentObjects = getDerivedParentObjectsForSymbol(State, Sym);
  if (RValObjects.isEmpty() && !RHSParentObjects.isEmpty() && Sym) {
    if (isGCObjectType(Sym->getType())) {
      if (GCObject Object = ensureObjectForSymbol(Sym, State, C)) {
        RValObjects = singletonObjectSet(State, Object);
        State = addObjectOwnershipEdges(State, RHSParentObjects, RValObjects);
        BoundDerivedObjects = true;
      }
    } else {
      State = setObjectsForSymbol(State, Sym, RHSParentObjects, C);
      RValObjects = RHSParentObjects;
      BoundDerivedObjects = true;
    }
  }
  if (IsRootingRegion || !LHSParentObjects.isEmpty()) {
    bool RValMayBeFreed = Sym && objectsMayBeFreed(State, RValObjects);
    if (!LHSParentObjects.isEmpty())
      State = addObjectRegionEdges(State, LHSParentObjects, R);
    State = bindRegionToObjects(State, R, RValObjects, C);
    if (RValMayBeFreed) {
      report_value_error(C, Sym, "Trying to root value which may have been GCed");
      if (IsRootingRegion) {
        for (GCObject Object : RValObjects)
          State = State->set<GCObjectStateMap>(Object, LivenessState::getAllocated());
      }
    }
    C.addTransition(State);
    return;
  }
  bool BoundTrackedStorage = false;
  if (const auto *TVR = R->getAs<TypedValueRegion>()) {
    if (isGCTrackedType(TVR->getValueType()) &&
        !Helpers::walk_back_to_global_VR(R)) {
      State = bindRegionToObjects(State, R, RValObjects, C);
      BoundTrackedStorage = true;
    }
  }
  if (!Sym) {
    if (BoundTrackedStorage || BoundDerivedObjects)
      C.addTransition(State);
    return;
  }
  if (!State->get<GCRootMap>(R)) {
    if (rootRegionIfGlobal(R->getBaseRegion(), State, C)) {
      C.addTransition(State);
      return;
    }
    if (BoundTrackedStorage || BoundDerivedObjects)
      C.addTransition(State);
    return;
  }
  std::optional<LivenessState> RValState = getStateForSymbol(State, Sym);
  if (!RValState) {
    if (rootRegionIfGlobal(Sym->getOriginRegion(), State, C)) {
      C.addTransition(State);
      return;
    }
    Sym->dump();
    if (auto *SC = dyn_cast<SymbolConjured>(Sym)) {
      SC->getStmt()->dump();
    }
    report_value_error(C, Sym,
                       "Saw assignment to root, but missed the allocation");
    return;
  }
  if (RValState->isPotentiallyFreed())
    report_value_error(C, Sym, "Trying to root value which may have been GCed");
  C.addTransition(State);
}

bool GCChecker::rootRegionIfGlobal(const MemRegion *R, ProgramStateRef &State,
                                   CheckerContext &C) const {
  if (!R)
    return false;
  const VarRegion *VR = R->getAs<VarRegion>();
  const MemRegion *RootRegion = R;
  if (!VR) {
    VR = Helpers::walk_back_to_global_VR(R);
    if (!VR)
      return false;
  }
  const VarDecl *VD = VR->getDecl();
  assert(VD);
  if (!VD->hasGlobalStorage())
    return false;
  const FieldDecl *GlobalField = nullptr;
  for (const MemRegion *Cur = RootRegion; Cur;) {
    if (const auto *FR = Cur->getAs<FieldRegion>()) {
      GlobalField = FR->getDecl();
      break;
    }
    const auto *SR = Cur->getAs<SubRegion>();
    if (!SR)
      break;
    Cur = SR->getSuperRegion();
  }
  bool isGlobalRoot =
      declHasAnnotation(VD, "julia_globally_rooted") ||
      isGloballyRootedType(VD->getType()) ||
      (GlobalField &&
       (declHasAnnotation(GlobalField, "julia_globally_rooted") ||
        isGloballyRootedType(GlobalField->getType())));
  bool isGlobalTracked =
      isGCTrackedType(VD->getType()) ||
      (GlobalField && isGCTrackedType(GlobalField->getType()));
  if (!isGlobalRoot && !isGlobalTracked)
    return false;
  if (isGlobalRoot) {
    State = State->set<GCRootMap>(RootRegion, -1);
    State = addPermanentRoot(State, RootRegion);
  }
  SVal TheVal = State->getSVal(RootRegion);
  SymbolRef Sym = TheVal.getAsSymbol();
  LivenessState TheValS = LivenessState::getAllocated();
  if (Sym) {
    GCObject Object = ensureObjectForSymbol(Sym, State, C, TheValS);
    if (Object) {
      const MemRegion *ObjectRegion = getObjectRegion(Object, C);
      State = setObjectsForRegion(State, RootRegion,
                                  singletonObjectSet(State, Object), C);
      if (ObjectRegion)
        State = setObjectsForRegion(State, ObjectRegion,
                                    singletonObjectSet(State, Object), C);
      if (isGlobalTracked)
        State = State->set<GCObjectStateMap>(Object, TheValS);
      if (isGlobalRoot) {
        State = addPermanentRoot(State, ObjectRegion);
        State = State->set<GCObjectStateMap>(Object, LivenessState::getAllocated());
      }
    }
  }
  return true;
}

void GCChecker::checkLocation(SVal SLoc, bool IsLoad, const Stmt *S,
                              CheckerContext &C) const {
  ProgramStateRef State = C.getState();
  bool DidChange = false;
  bool IsRootRegion = isRootingRegion(State, SLoc.getAsRegion()) ||
                      State->get<GCRootMap>(SLoc.getAsRegion());
  if (IsLoad && IsRootRegion) {
    auto LoadLoc = SLoc.getAs<Loc>();
    if (!LoadLoc)
      return;
    SVal Loaded = State->getSVal(*LoadLoc);
    if (Loaded.isUnknown() || !Loaded.getAsSymbol()) {
      if (const auto *E = dyn_cast_or_null<Expr>(S)) {
        if (isGCTracked(E)) {
          Loaded = C.getSValBuilder().conjureSymbolVal(
              nullptr, C.getCFGElementRef(), C.getLocationContext(),
              E->getType(), C.blockCount());
          State = State->bindLoc(*LoadLoc, Loaded, C.getLocationContext());
          DidChange = true;
        }
      }
    }
    SymbolRef LoadedSym = Loaded.getAsSymbol();
    if (LoadedSym) {
      GCObjectSet LoadedObjects =
          getObjectsForRegion(State, SLoc.getAsRegion());
      if (LoadedObjects.isEmpty()) {
        if (GCObject Object = ensureObjectForSymbol(LoadedSym, State, C))
          LoadedObjects = singletonObjectSet(State, Object);
      } else {
        State = setObjectsForSymbol(State, LoadedSym, LoadedObjects, C);
      }
      State = bindRegionToSVal(State, SLoc.getAsRegion(), Loaded, C);
      LoadedObjects = getObjectsForRegion(State, SLoc.getAsRegion());
      if (LoadedObjects.isEmpty())
        LoadedObjects = getObjectsForSymbol(State, LoadedSym);
      DidChange = true;
    }
  }
  // If it's just the symbol by itself, let it be. We allow dead pointers to be
  // passed around, so long as they're not accessed. However, we do want to
  // start tracking any globals that may have been accessed.
  if (rootRegionIfGlobal(SLoc.getAsRegion(), State, C)) {
    if (IsLoad && isRootingRegion(State, SLoc.getAsRegion())) {
      if (auto LoadLoc = SLoc.getAs<clang::ento::Loc>()) {
        SVal Loaded = State->getSVal(*LoadLoc);
        if (SymbolRef LoadedSym = Loaded.getAsSymbol()) {
          ensureObjectForSymbol(LoadedSym, State, C);
          State = bindRegionToSVal(State, SLoc.getAsRegion(), Loaded, C);
        }
      }
    }
    C.addTransition(State);
    return;
  }
  bool AccessesGCTrackedValue = false;
  if (const auto *E = dyn_cast_or_null<Expr>(S))
    AccessesGCTrackedValue = isGCTracked(E);
  if (IsLoad && AccessesGCTrackedValue) {
    const MemRegion *LoadedRegion = SLoc.getAsRegion();
    GCObjectSet OwnerObjects = getTrackedParentObjects(State, LoadedRegion);
    if (!OwnerObjects.isEmpty()) {
      if (auto LoadLoc = SLoc.getAs<Loc>()) {
        SVal Loaded = State->getSVal(*LoadLoc);
        QualType LoadedType;
        if (const auto *E = dyn_cast_or_null<Expr>(S))
          LoadedType = getAtomicValueType(E->getType());
        if ((Loaded.isUnknown() || !Loaded.getAsSymbol()) &&
            !LoadedType.isNull() && isGCTrackedType(LoadedType)) {
          Loaded = C.getSValBuilder().conjureSymbolVal(
              nullptr, C.getCFGElementRef(), C.getLocationContext(),
              LoadedType, C.blockCount());
          State = State->bindLoc(*LoadLoc, Loaded, C.getLocationContext());
        }
        GCObjectSet LoadedObjects = getObjectsForSVal(State, Loaded);
        if (LoadedObjects.isEmpty()) {
          if (SymbolRef LoadedSym = Loaded.getAsSymbol(true)) {
            if (GCObject Object =
                    ensureObjectForSymbol(LoadedSym, State, C,
                                          aggregateObjectState(State,
                                                               OwnerObjects),
                                          LoadedType))
              LoadedObjects = singletonObjectSet(State, Object);
          }
        }
        if (!LoadedObjects.isEmpty()) {
          State = addObjectRegionEdges(State, OwnerObjects, LoadedRegion);
          State = bindRegionToObjects(State, LoadedRegion, LoadedObjects, C);
          DidChange = true;
        }
      }
    }
  }
  std::optional<GCObjectSet> Reachable;
  auto getReachable = [&]() {
    if (!Reachable)
      Reachable = computeReachableObjects(State);
    return *Reachable;
  };
  if (IsLoad && AccessesGCTrackedValue) {
    GCObjectSet OwnerObjects =
        getTrackedParentObjects(State, SLoc.getAsRegion());
    GCObjectSet CurrentReachable = getReachable();
    if (!OwnerObjects.isEmpty() &&
        objectsMayBeFreed(State, OwnerObjects, CurrentReachable)) {
      GCObjectSet DirectObjects = emptyObjectSet(State);
      for (const MemRegion *Cur =
               SLoc.getAsRegion() ? SLoc.getAsRegion()->StripCasts() : nullptr;
           Cur;) {
        GCObjectSet CurObjects = getObjectsForRegion(State, Cur);
        for (GCObject Object : CurObjects)
          DirectObjects =
              State->get_context<GCObjectSet>().add(DirectObjects, Object);
        if (!DirectObjects.isEmpty())
          break;
        const auto *SR = Cur->getAs<SubRegion>();
        if (!SR)
          break;
        Cur = SR->getSuperRegion()->StripCasts();
      }
      if (!DirectObjects.isEmpty() &&
          !objectsMayBeFreed(State, DirectObjects, CurrentReachable)) {
        DidChange &&C.addTransition(State);
        return;
      }
      report_value_error(C, *OwnerObjects.begin(),
                         "Trying to access value which may have been GCed");
      return;
    }
  }
  SymbolRef SymByItself = SLoc.getAsSymbol(false);
  if (SymByItself) {
    DidChange &&C.addTransition(State);
    return;
  }
  // This will walk backwards until it finds the base symbol
  SymbolRef Sym = SLoc.getAsSymbol(true);
  if (!Sym) {
    DidChange &&C.addTransition(State);
    return;
  }
  bool PreferDerivativeDiagnostic = isa_and_nonnull<ArraySubscriptExpr>(S);
  if (const auto *UO = dyn_cast_or_null<UnaryOperator>(S))
    PreferDerivativeDiagnostic = PreferDerivativeDiagnostic ||
                                 UO->getOpcode() == UO_Deref;
  if (PreferDerivativeDiagnostic) {
    DidChange &&C.addTransition(State);
    return;
  }
  std::optional<LivenessState> VState = getStateForSymbol(State, Sym);
  if (!VState) {
    DidChange &&C.addTransition(State);
    return;
  }
  if (VState->isPotentiallyFreed()) {
    report_value_error(C, Sym,
                       "Trying to access value which may have been GCed");
  } else if (!VState->isUntracked()) {
    GCObjectSet Objects = getObjectsForSymbol(State, Sym);
    if (objectsMayBeFreed(State, Objects, getReachable())) {
      report_value_error(C, Sym,
                         "Trying to access value which may have been GCed");
    }
  }
  DidChange &&C.addTransition(State);
}

} // namespace jl_gc_checker
