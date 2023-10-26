include "llvm-dialects/Dialect/Dialect.td"

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

