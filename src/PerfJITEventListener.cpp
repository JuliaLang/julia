#include <llvm/Object/SymbolSize.h>
#include <unistd.h>

#define DEBUG_TYPE "perf-jit-event-listener"

namespace {

using namespace llvm::object;

class PerfJITEventListener : public JITEventListener {
  raw_fd_ostream *OS;
  bool enabled;

  void initialize();
  std::map<const char*, OwningBinary<ObjectFile>> DebugObjects;

public:
  PerfJITEventListener() {
    initialize();
  }

  ~PerfJITEventListener();

  void NotifyObjectEmitted(const ObjectFile &Obj,
                           const RuntimeDyld::LoadedObjectInfo &L) override;

  void NotifyFreeingObject(const ObjectFile &Obj) override;
};

void PerfJITEventListener::initialize() {
  std::stringstream fname;
  fname << "/tmp/perf-";
  fname << getpid() << ".map";
  std::error_code err;
  OS = new raw_fd_ostream(fname.str(), err, sys::fs::F_Excl | sys::fs::F_Text);
  if (err) {
    jl_printf(JL_STDERR, "WARNING: could not open perf symbol mapping (%s)\n", err.message().c_str());
  } else {
    enabled = true;
  }
}

PerfJITEventListener::~PerfJITEventListener() {
  if (enabled) {
    OS->close();
  }
}

void PerfJITEventListener::NotifyObjectEmitted(
                                       const ObjectFile &Obj,
                                       const RuntimeDyld::LoadedObjectInfo &L) {
  if (!enabled) {
    return;
  }

  OwningBinary<ObjectFile> DebugObjOwner = L.getObjectForDebug(Obj);
  const ObjectFile &DebugObj = *DebugObjOwner.getBinary();

  // Use symbol info to iterate functions in the object.
  for (const std::pair<SymbolRef, uint64_t> &P : object::computeSymbolSizes(DebugObj)) {
    SymbolRef Sym = P.first;
    if (Sym.getType() != SymbolRef::ST_Function)
      continue;

    ErrorOr<StringRef> NameOrErr = Sym.getName();
    if (NameOrErr.getError())
      continue;
    StringRef Name = *NameOrErr;
    ErrorOr<uint64_t> AddrOrErr = Sym.getAddress();
    if (AddrOrErr.getError())
      continue;
    uint64_t Addr = *AddrOrErr;
    uint64_t Size = P.second;

    OS->write_hex(Addr);
    *OS << " ";
    OS->write_hex(Size);
    *OS << " " << Name.data() << "\n";

    // NOTE: perf doesn't currently support annotating JITted code.
    //       When it does (https://lwn.net/Articles/638566/), we should
    //       emit source location information (see IntelJITEventListener.cpp)

    // FIXME: our destructor isn't called, so flush manually for each entry
    OS->flush();
  }

  DebugObjects[Obj.getData().data()] = std::move(DebugObjOwner);
}

void PerfJITEventListener::NotifyFreeingObject(const ObjectFile &Obj) {
  if (enabled) {
    // FIXME: does this ever happen, apart from when destroying the runtime?
  }

  DebugObjects.erase(Obj.getData().data());
}

}  // anonymous namespace.

JITEventListener *createPerfJITEventListener() {
  return new PerfJITEventListener();
}
