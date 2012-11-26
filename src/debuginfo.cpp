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
