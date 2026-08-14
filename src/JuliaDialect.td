// This file is a part of Julia. License is MIT: https://julialang.org/license

// Specification of the Julia LLVM dialect.
//
// Julia's codegen emits pseudo-intrinsics and non-integral address spaces on
// top of plain LLVM IR to express Julia-specific semantics, in particular the
// value tracking required for a precise, moving GC. This file is the single
// authoritative specification of those operations: llvm-dialects-tblgen
// generates op classes (JuliaDialect.h.inc / JuliaDialect.cpp.inc) that are
// used by codegen to build the ops and by the optimization passes to
// recognize them.
//
// The ops in this dialect are legal from codegen until the late GC lowering
// pass (LateLowerGCFrame), which together with final GC lowering legalizes
// the module down to plain LLVM IR that the backends can consume.

include "llvm-dialects/Dialect/Dialect.td"

def JuliaDialect : Dialect {
  let name = "julia";
  let cppNamespace = "julia";
}

def JuliaValue : DialectType<JuliaDialect, "jlvalue"> {
  let typeArguments = (args);

  let defaultGetterHasExplicitContextArgument = true;

  let summary = "an opaque Julia value";
  let description = [{
    An opaque, GC-managed Julia value. Pointers to `jlvalue` in the tracked
    address space are rooted by the GC frame lowering.
  }];
}

// Address spaces used by the Julia dialect to track GC-managed pointers.
// These are non-integral address spaces (see the datalayout string in
// codegen): pointers in them may be rewritten by the GC lowering passes and
// must not be introspected as integers.
defm AttrJuliaAddressSpace : AttrEnum<"AddressSpace">;
def Generic : CppConstant<"julia::AddressSpace::Generic">;
def Tracked : CppConstant<"julia::AddressSpace::Tracked">;
def Derived : CppConstant<"julia::AddressSpace::Derived">;
def CalleeRooted : CppConstant<"julia::AddressSpace::CalleeRooted">;
def Loaded : CppConstant<"julia::AddressSpace::Loaded">;

def GenericPtr : TgConstant<(PointerType Generic)>, Type;
def TrackedPtr : TgConstant<(PointerType Tracked)>, Type;
def DerivedPtr : TgConstant<(PointerType Derived)>, Type;
def CalleeRootedPtr : TgConstant<(PointerType CalleeRooted)>, Type;
def LoadedPtr : TgConstant<(PointerType Loaded)>, Type;

class JuliaOp<string mnemonic_, list<Trait> traits_>
    : Op<JuliaDialect, mnemonic_, traits_>;

def GetPGCStack : JuliaOp<"get_pgcstack",
                       [Memory<[(read InaccessibleMem)]>, NoUnwind]> {
  let results = (outs Ptr:$result);
  let arguments = (ins);

  let summary = "obtain the hidden pgcstack pointer of the current task";
  let description = [{
    Placeholder for obtaining the current task's GC stack pointer. Emitted by
    codegen in the entry block of every function that may need a GC frame;
    lowered by the `LowerPTLS` pass to the actual thread-local lookup, or
    deleted if no GC frame turns out to be required.
  }];
}

def GetPGCStackOrNew : JuliaOp<"get_pgcstack_or_new",
                       [Memory<[(read InaccessibleMem)]>, NoUnwind]> {
  let results = (outs Ptr:$result);
  let arguments = (ins);

  let summary = "obtain the pgcstack pointer, adopting the thread if needed";
  let description = [{
    Like `julia.get_pgcstack`, but adopts the current thread into the Julia
    runtime if it is not yet known to it. Used for `@ccallable` entry points
    that may be called from foreign threads.
  }];
}

// Attribute traits not (yet) provided by llvm-dialects upstream.
def NoAlias : LlvmEnumAttributeTrait<"NoAlias", [ParamTrait, RetTrait]>;

def GCLoaded : JuliaOp<"gc_loaded",
                    [Memory<[]>, NoSync, NoUnwind, Speculatable, WillReturn, NoRecurse]> {
  let results = (outs LoadedPtr:$result);
  let arguments = (ins TrackedPtr:$base, Ptr:$tracked);

  let value_traits = [
    (NonNull $result), (NoUndef $result),
    (NonNull $base), (NoUndef $base), (NoCapture $base),
    (NonNull $tracked), (NoUndef $tracked),
  ];

  let summary = "associate a loaded interior pointer with its GC base object";
  let description = [{
    Records that `tracked` is an interior pointer into the GC-managed object
    `base` (e.g. the data pointer loaded from a `Memory` object). The result
    is `tracked`, viewed in the `Loaded` address space, so that the GC
    lowering can keep `base` alive across safepoints while the interior
    pointer is in use. Lowered by `LateLowerGCFrame` to an addrspacecast of
    `tracked`.
  }];
}

// ============================================================================
// GC lowering ops ("Julia GC dialect")
//
// The ops below form a second layer of the dialect: they are *produced* by
// the LateLowerGCFrame pass when it lowers the ops above, and *consumed* by
// the FinalLowerGC pass, which replaces them with concrete runtime calls and
// GC frame manipulation. They are not legal before LateLowerGCFrame or after
// FinalLowerGC.
//
// They currently share the `julia.` prefix with the codegen-level ops for
// backwards compatibility of the textual IR; producers must respect the
// legality phases described here.
// ============================================================================

def NewGCFrame : JuliaOp<"new_gc_frame", [NoUnwind]> {
  let results = (outs Ptr:$frame);
  let arguments = (ins I32:$size);

  let value_traits = [
    (NoAlias $frame), (NonNull $frame),
  ];

  let summary = "allocate a new GC frame with `size` root slots";
  let description = [{
    Allocates a GC frame with `size` slots for GC roots. Lowered by
    `FinalLowerGC` to an alloca (plus the frame header).
  }];
}

def PushGCFrame : JuliaOp<"push_gc_frame", [NoUnwind]> {
  let results = (outs);
  let arguments = (ins Ptr:$frame, I32:$size);

  let summary = "link a GC frame into the current task's GC stack";
  let description = [{
    Initializes the header of `frame` (with `size` root slots) and pushes it
    onto the current task's chain of GC frames.
  }];
}

def PopGCFrame : JuliaOp<"pop_gc_frame", [NoUnwind]> {
  let results = (outs);
  let arguments = (ins Ptr:$frame);

  let summary = "unlink a GC frame from the current task's GC stack";
  let description = [{}];
}

def GetGCFrameSlot : JuliaOp<"get_gc_frame_slot", [Memory<[]>, NoUnwind]> {
  let results = (outs Ptr:$slot);
  let arguments = (ins Ptr:$frame, I32:$index);

  let summary = "address of root slot `index` in a GC frame";
  let description = [{}];
}

def GCAllocBytes : JuliaOp<"gc_alloc_bytes",
                        [Memory<[(read ArgMem), (readwrite InaccessibleMem)]>,
                         NoUnwind, WillReturn]> {
  let results = (outs TrackedPtr:$result);
  let arguments = (ins Ptr:$ptls, (IntegerType any):$size, (IntegerType any):$type);

  let value_traits = [
    (NoAlias $result), (NonNull $result),
  ];

  let summary = "allocate a GC-managed object of `size` bytes";
  let description = [{
    Allocates `size` bytes (not including the object header) with object tag
    `type`. Lowered by `FinalLowerGC` to a call to the appropriate runtime
    allocation function based on the size. The declaration additionally
    carries `allocsize(1)` and `allockind("alloc")` attributes, which cannot
    yet be expressed as llvm-dialects traits.
  }];
}

def QueueGCRoot : JuliaOp<"queue_gc_root",
                       [Memory<[(readwrite ArgMem), (readwrite InaccessibleMem)]>,
                        NoUnwind]> {
  let results = (outs);
  let arguments = (ins TrackedPtr:$root);

  let summary = "re-queue an old object for GC scanning (write barrier slow path)";
  let description = [{}];
}

def Safepoint : JuliaOp<"safepoint",
                     [Memory<[(readwrite ArgMem), (readwrite InaccessibleMem)]>,
                      NoUnwind]> {
  let results = (outs);
  let arguments = (ins Ptr:$signal_page);

  let summary = "GC safepoint: potential stop-the-world synchronization point";
  let description = [{
    Loads from the GC signal page so that stopping the world can be triggered
    by protecting it.
  }];
}
