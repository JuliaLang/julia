// --- storing and accessing source location metadata ---

struct FuncInfo{
    const Function* func;
    size_t lengthAdr;
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> lines;
};

class JuliaJITEventListener: public JITEventListener
{
    std::map<size_t, FuncInfo> info;
    
public:	
    JuliaJITEventListener(){}
    virtual ~JuliaJITEventListener() {}
    
    virtual void NotifyFunctionEmitted(const Function &F, void *Code,
                                       size_t Size, const EmittedFunctionDetails &Details)
    {
        FuncInfo tmp = {&F, Size, Details.LineStarts};
        info[(size_t)(Code)] = tmp;
#if defined(_OS_WINDOWS_)
        PRUNTIME_FUNCTION tbl = (PRUNTIME_FUNCTION)(((uintptr_t)Code + Size + 3)&~(uintptr_t)3);
        uint8_t *UnwindData = (uint8_t*)((((uintptr_t)&tbl[1])+3)&~(uintptr_t)3);
        RUNTIME_FUNCTION fn = {0,(DWORD)Size,(DWORD)(intptr_t)(UnwindData-(uint8_t*)Code)};
        tbl[0] = fn;
        UnwindData[0] = 0x20; // version info, UNW_FLAG_NHANDLER
        UnwindData[1] = 4;    // size of prolog (bytes)
        UnwindData[2] = 2;    // count of unwind codes (slots)
        UnwindData[3] = 0x50; // frame register (rbp) = rsp
        UnwindData[4] = 0x01; // first instruction
        UnwindData[5] = 0x05; // push RBP
        UnwindData[6] = 0x04; // second instruction
        UnwindData[7] = 0x10; // mov RBP, RSP
        RtlAddFunctionTable(tbl,1,(DWORD64)Code);
#endif
    }
    
    std::map<size_t, FuncInfo>& getMap()
    {
        return info;
    }
};

JuliaJITEventListener *jl_jit_events;

extern "C" void getFunctionInfo(const char **name, int *line, const char **filename,size_t pointer);

void getFunctionInfo(const char **name, int *line, const char **filename, size_t pointer)
{
    std::map<size_t, FuncInfo> &info = jl_jit_events->getMap();
    *name = NULL;
    *line = -1;
    *filename = "no file";
    for (std::map<size_t, FuncInfo>::iterator it= info.begin(); it!= info.end(); it++) {
        if ((*it).first <= pointer) {
            if ((size_t)(*it).first + (*it).second.lengthAdr >= pointer) {
                // commenting these lines out skips functions that don't
                // have explicit debug info. this is useful for hiding
                // the jlcall wrapper functions we generate.
#if LLVM_VERSION_MAJOR == 3
#if LLVM_VERSION_MINOR == 0
                //*name = &(*(*it).second.func).getNameStr()[0];
#elif LLVM_VERSION_MINOR >= 1
                //*name = (((*(*it).second.func).getName()).data());
#endif
#endif
                if ((*it).second.lines.size() == 0) {
                    continue;
                }
                
                std::vector<JITEvent_EmittedFunctionDetails::LineStart>::iterator vit = (*it).second.lines.begin();
                JITEvent_EmittedFunctionDetails::LineStart prev = *vit;

                DISubprogram debugscope =
                    DISubprogram(prev.Loc.getScope((*it).second.func->getContext()));
                *filename = debugscope.getFilename().data();
                // the DISubprogram has the un-mangled name, so use that if
                // available.
                *name = debugscope.getName().data();
                
                vit++;
                
                while (vit != (*it).second.lines.end()) {
                    if (pointer <= (*vit).Address) {
                        *line = prev.Loc.getLine();
                        break;
                    }
                    prev = *vit;
                    vit++;
                }
                if (*line == -1) {
                    *line = prev.Loc.getLine();
                }
                
                break;
            }
        }
    }
}

#if defined(_OS_WINDOWS_)
class JITMemoryManagerWin : public JITMemoryManager {
private:
  JITMemoryManager *JMM;
public:
  JITMemoryManagerWin() : JITMemoryManager() {
      JMM = JITMemoryManager::CreateDefaultMemManager();
  }
  virtual void setMemoryWritable() { return JMM->setMemoryWritable(); }
  virtual void setMemoryExecutable() { return JMM->setMemoryExecutable(); }
  virtual void setPoisonMemory(bool poison) { return JMM->setPoisonMemory(poison); }
  virtual void AllocateGOT() { JMM->AllocateGOT(); HasGOT = true; }
  virtual uint8_t *getGOTBase() const { return JMM->getGOTBase(); }
  virtual uint8_t *startFunctionBody(const Function *F,
                                     uintptr_t &ActualSize) { ActualSize += 16; uint8_t *ret = JMM->startFunctionBody(F,ActualSize); ActualSize -= 16; return ret; }
  virtual uint8_t *allocateStub(const GlobalValue* F, unsigned StubSize,
                                unsigned Alignment)  { return JMM->allocateStub(F,StubSize,Alignment); }
  virtual void endFunctionBody(const Function *F, uint8_t *FunctionStart,
                               uint8_t *FunctionEnd) { return JMM->endFunctionBody(F,FunctionStart,FunctionEnd+16); }
  virtual uint8_t *allocateSpace(intptr_t Size, unsigned Alignment) { return JMM->allocateSpace(Size,Alignment); }
  virtual uint8_t *allocateGlobal(uintptr_t Size, unsigned Alignment) { return JMM->allocateGlobal(Size,Alignment); }
  virtual void deallocateFunctionBody(void *Body) { return JMM->deallocateFunctionBody(Body); }
  virtual uint8_t* startExceptionTable(const Function* F,
                                       uintptr_t &ActualSize) { return JMM->startExceptionTable(F,ActualSize); }
  virtual void endExceptionTable(const Function *F, uint8_t *TableStart,
                                 uint8_t *TableEnd, uint8_t* FrameRegister) { return JMM->endExceptionTable(F,TableStart,TableEnd,FrameRegister); }
  virtual void deallocateExceptionTable(void *ET) { return JMM->deallocateExceptionTable(ET); }
  virtual bool CheckInvariants(std::string &str) { return JMM->CheckInvariants(str); }
  virtual size_t GetDefaultCodeSlabSize() { return JMM->GetDefaultCodeSlabSize(); }
  virtual size_t GetDefaultDataSlabSize() { return JMM->GetDefaultDataSlabSize(); }
  virtual size_t GetDefaultStubSlabSize() { return JMM->GetDefaultStubSlabSize(); }
  virtual unsigned GetNumCodeSlabs() { return JMM->GetNumCodeSlabs(); }
  virtual unsigned GetNumDataSlabs() { return JMM->GetNumDataSlabs(); }
  virtual unsigned GetNumStubSlabs() { return JMM->GetNumStubSlabs(); }

  virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID) { return JMM->allocateCodeSection(Size,Alignment,SectionID); }
  virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID, bool IsReadOnly) { return JMM->allocateDataSection(Size,Alignment,SectionID,IsReadOnly); }
  virtual void *getPointerToNamedFunction(const std::string &Name,
                                          bool AbortOnFailure = true) { return JMM->getPointerToNamedFunction(Name,AbortOnFailure); }
  virtual bool applyPermissions(std::string *ErrMsg = 0) { return JMM->applyPermissions(ErrMsg); }
  virtual void registerEHFrames(StringRef SectionData) { return JMM->registerEHFrames(SectionData); }
};
#endif
