#include <llvm/ExecutionEngine/JITEventListener.h>
#include "llvm/Function.h"
#include <map>
#include <llvm/Analysis/DebugInfo.h>

using namespace std;
using namespace llvm;

struct FuncInfo{
    const Function* func;
    //const char* funcName;
    size_t lengthAdr;
    vector<JITEvent_EmittedFunctionDetails::LineStart> lines;
    //uint linenum;
    //uint colnum;
};


class JuliaJITEventListener: public JITEventListener
{
    map<size_t, FuncInfo> info;
    
public:	
    JuliaJITEventListener(){}
    virtual ~JuliaJITEventListener() {}
    
    virtual void NotifyFunctionEmitted(const Function &F, 
                                       void *Code, size_t Size, const EmittedFunctionDetails &Details) {
        //printf("Vector lengths %lx \n",Details.LineStarts.size());
        
        
        //printf("print vectors sizes going in: %d \n", Details.LineStarts.size());
        FuncInfo tmp = {&F, Size, Details.LineStarts /*.Loc.getLine(),10/*, Details.Loc.getCol()*/};
        info[(size_t)(Code)] = tmp;
        
        /*
        //Debug use. 
        for(std::vector<JITEvent_EmittedFunctionDetails::LineStart>::const_iterator vit
        = Details.LineStarts.begin(); vit != Details.LineStarts.end(); ++vit) {
        printf("this vector is being added %lx \n",(*vit).Address);
        
        }
        */
        
    }
    
    map<size_t, FuncInfo> getMap() {
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
    *filename = "Not found";
    //printf("funcinfo???\n");
    for (map<size_t, FuncInfo>::iterator it= info.begin(); it!= info.end(); it++) {
        if ((*it).first <= pointer) {
            if ( (size_t)(*it).first + (*it).second.lengthAdr >= pointer) {
                //toReturn = (*(*it).second.func).getName().data();
                //here only for debug purposes, should not be instatiated twice. 
                //*name = (*it).second.func.getName().data();
                *name = &(*(*it).second.func).getNameStr()[0];
                //DIDescriptor temp = DIDescriptor((*(*it).second.func))
                //printf("la %i \n", (*(*it).second.func).getSubclassDataFromValue());
                //printf("start addr %lx\n", (size_t)(*it).first);
                //printf("print vector size: %d \n", (*it).second.lines.size());
                if ((*it).second.lines.size() == 0) {
                	//printf("no line number info\n");
                    continue;
                }
                
                std::vector<JITEvent_EmittedFunctionDetails::LineStart>::iterator vit = (*it).second.lines.begin();
                JITEvent_EmittedFunctionDetails::LineStart prev = *vit;
                //printf("vector %lx \n",(*vit).Address);
                vit++;
                
                while (vit != (*it).second.lines.end()) {
                    //printf("vector %lx \n",(*vit).Address);
                    if (pointer < (*vit).Address) {
                        //*name = ((Function)(*it).second.func)->getName().data();
                        *line = prev.Loc.getLine();
                        //printf("banananans\n");
              			DIScope please = DIScope(prev.Loc.getScope((*(*it).second.func).getContext()));
                                *filename = please.getFilename().data();

                        //prev.Loc.getScope(getGlobalContext())->dump();
                        //printf("exiting vector loop\n");
                        break;
                    }
                    prev = *vit;
                    vit++;
                } 
                if (*line == -1) {
                    // must be last line, since we know ip is in this function
                    //printf("apricots\n");
              		DIScope please = DIScope(prev.Loc.getScope((*(*it).second.func).getContext()));
                	*filename = please.getFilename().data();
                	//printf("directry is : %s\n", please.getDirectory().data());
                    *line = prev.Loc.getLine();
                }
                
                break;
            }
        }
    }
}
