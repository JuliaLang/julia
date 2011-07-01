#include <llvm/ExecutionEngine/JITEventListener.h>
#include "llvm/Function.h"
#include <map>

using namespace std;
using namespace llvm;

struct FuncInfo{
	const Function* func;
	size_t lengthAdr;
};


class JuliaJITEventListener: public JITEventListener {
	map<size_t, FuncInfo> info;

public:	
	JuliaJITEventListener(){}
	virtual ~JuliaJITEventListener() {}

	virtual void NotifyFunctionEmitted(const Function &F, 
		void *Code, size_t Size, const EmittedFunctionDetails &Details) {
		FuncInfo tmp = {&F, Size};
		info[(size_t)(Code)] = tmp;
	}

	map<size_t, FuncInfo> getMap() {
		return info;
	}
};

extern JuliaJITEventListener *jl_jit_events;

extern "C" const char* getFunctionInfo(size_t pointer);

const char* getFunctionInfo(size_t pointer) {
		map<size_t, FuncInfo> info = jl_jit_events->getMap();
		const char* toReturn = NULL;
		for (map<size_t, FuncInfo>::iterator it= info.begin(); it!= info.end(); it++) {
			if ((*it).first <= pointer) {
				if ( (size_t)(*it).first + (*it).second.lengthAdr >= pointer) {
					 toReturn = (*(*it).second.func).getName().data();
					 break;
				}
			}
		}
		return toReturn;
	}