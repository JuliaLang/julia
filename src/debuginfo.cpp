#include <llvm/ExecutionEngine/JITEventListener.h>
#include "llvm/Function.h"
#include <map>
#include <llvm/Analysis/DebugInfo.h>

using namespace std;
using namespace llvm;

struct FuncInfo{
    const Function* func;
    size_t lengthAdr;
    vector<JITEvent_EmittedFunctionDetails::LineStart> lines;
};


class JuliaJITEventListener: public JITEventListener
{
    map<size_t, FuncInfo> info;
    
public:	
    JuliaJITEventListener(){}
    virtual ~JuliaJITEventListener() {}
    
    virtual void NotifyFunctionEmitted(const Function &F, 
                                       void *Code, size_t Size, const EmittedFunctionDetails &Details)
    {
        FuncInfo tmp = {&F, Size, Details.LineStarts};
        info[(size_t)(Code)] = tmp;
    }
    
    map<size_t, FuncInfo> getMap()
    {
        return info;
    }
};

extern JuliaJITEventListener *jl_jit_events;

extern "C" void getFunctionInfo(char **name, int *line, const char **filename,size_t pointer);

void getFunctionInfo(char **name, int *line, const char **filename, size_t pointer)
{
    map<size_t, FuncInfo> info = jl_jit_events->getMap();
    *name = NULL;
    *line = -1;
    *filename = "unknown";
    for (map<size_t, FuncInfo>::iterator it= info.begin(); it!= info.end(); it++) {
        if ((*it).first <= pointer) {
            if ( (size_t)(*it).first + (*it).second.lengthAdr >= pointer) {
                *name = &(*(*it).second.func).getNameStr()[0];
                if ((*it).second.lines.size() == 0) {
                    continue;
                }
                
                std::vector<JITEvent_EmittedFunctionDetails::LineStart>::iterator vit = (*it).second.lines.begin();
                JITEvent_EmittedFunctionDetails::LineStart prev = *vit;
                vit++;
                
                while (vit != (*it).second.lines.end()) {
                    if (pointer < (*vit).Address) {
                        *line = prev.Loc.getLine();
                        DIScope debugscope =
                            DIScope(prev.Loc.getScope((*it).second.func->getContext()));
                        *filename = debugscope.getFilename().data();
                        break;
                    }
                    prev = *vit;
                    vit++;
                } 
                if (*line == -1) {
                    DIScope debugscope =
                        DIScope(prev.Loc.getScope((*it).second.func->getContext()));
                    *filename = debugscope.getFilename().data();
                    *line = prev.Loc.getLine();
                }
                
                break;
            }
        }
    }
}
