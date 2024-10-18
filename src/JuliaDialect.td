include "llvm-dialects/Dialect/Dialect.td"

def Speculatable : LlvmEnumAttributeTrait<"Speculatable">;


def NonNull : LlvmEnumAttributeTrait<"NonNull">;
def NoUndef : LlvmEnumAttributeTrait<"NoUndef">;
def NoCapture : LlvmEnumAttributeTrait<"NoCapture">;
def ReadNone : LlvmEnumAttributeTrait<"ReadNone">;

def JuliaDialect : Dialect {
  let name = "julia";
  let cppNamespace = "julia";
}

def JuliaValue : DialectType<JuliaDialect, "jlvalue"> {
  let typeArguments = (args);

  let defaultGetterHasExplicitContextArgument = true;

  let summary = "an opaque Julia type";
  let description = [{}];
}

defm AttrJuliaAddressSpace : AttrEnum<"AddressSpace">;
def Generic : CppConstant<"julia::AddressSpace::Generic">;
def Tracked : CppConstant<"julia::AddressSpace::Tracked">;
def Derived : CppConstant<"julia::AddressSpace::Derived">;
def CalleeRooted : CppConstant<"julia::AddressSpace::CalleeRooted">;
def Loaded : CppConstant<"julia::AddressSpace::Loaded">;

def GenericPtr: TgConstant<(PointerType Generic)>, Type;
def TrackedPtr: TgConstant<(PointerType Tracked)>, Type;
def DerivedPtr: TgConstant<(PointerType Derived)>, Type;
def CalleeRootedPtr: TgConstant<(PointerType CalleeRooted)>, Type;
def LoadedPtr: TgConstant<(PointerType Loaded)>, Type;

class JuliaOp<string mnemonic_, list<Trait> traits_>
    : Op<JuliaDialect, mnemonic_, traits_>;

def GetPGCStack : JuliaOp<"get_pgcstack",
                       [Memory<[(read InaccessibleMem)]>, NoUnwind]> {
  let results = (outs Ptr:$result);
  let arguments = (ins);

  let summary = "Obtain hidden pgcstack pointer";
  let description = [{}];
}

def GetPGCStackOrNew : JuliaOp<"get_pgcstack_or_new",
                       [Memory<[(read InaccessibleMem)]>, NoUnwind]> {
  let results = (outs Ptr:$result);
  let arguments = (ins);

  let summary = "Obtain hidden pgcstack pointer";
  let description = [{}];
}

def GCLoaded : JuliaOp<"gc_loaded",
                    [Memory<[]>, NoSync, NoUnwind, Speculatable, WillReturn, NoRecurse]> {
    let results = (outs LoadedPtr:$result);
    let arguments = (ins TrackedPtr:$base, Ptr:$tracked);
    let summary = "";
    let description = [{}];
}

    // declare nonnull noundef ptr(Loaded) @"julia.gc_loaded"(ptr(Tracked) nocapture nonnull noundef readnone, ptr nonnull noundef readnone)


//         AttrBuilder RetAttrs(C);
//         RetAttrs.addAttribute(Attribute::NonNull);
//         RetAttrs.addAttribute(Attribute::NoUndef);
//         return AttributeList::get(C, AttributeSet::get(C,FnAttrs), AttributeSet::get(C,RetAttrs),
//                 { Attributes(C, {Attribute::NonNull, Attribute::NoUndef, Attribute::ReadNone, Attribute::NoCapture}),
//                   Attributes(C, {Attribute::NonNull, Attribute::NoUndef, Attribute::ReadNone}) });
//                   },
// };