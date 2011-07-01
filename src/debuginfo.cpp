#include <llvm/ExecutionEngine/JITEventListener.h>
#include "llvm/Function.h"
#include <map>

using namespace std;
using namespace llvm;

struct FuncInfo{
	const Function* func;
	size_t lengthAdr;
};


class __attribute__ ((visibility("default"))) JuliaJITEventListener: public JITEventListener {
	map<size_t, FuncInfo> info;

public:	
	virtual void NotifyFunctionEmitted(const Function &F, 
		void *Code, size_t Size, const EmittedFunctionDetails &Details) {
		// Here i should write down the information into the map i want to know. Important Code name, location of code start, length (and or end),
		FuncInfo tmp = {&F, Size};
		//tmp.func = &F;
		//tmp.lengthAdr = Size;
		info[(size_t)(Code)] = tmp;
	}
	JuliaJITEventListener(){}
	virtual ~JuliaJITEventListener() {}
	public:

		map<size_t, FuncInfo> getMap() {
			return info;

		}

	

};

extern JuliaJITEventListener *jl_jit_events;

extern "C" const char* getFunctionInfo(size_t pointer);

///*
const char* getFunctionInfo(size_t pointer) {
		//printf("entered getfunc ");
		map<size_t, FuncInfo> info = jl_jit_events->getMap();
		const char* toReturn = "not found";
		for (map<size_t, FuncInfo>::iterator it= info.begin(); it!= info.end(); it++) {
			//printf("looking at pointer %lx ", (*it).first);
			if ((*it).first <= pointer) {
				//printf("start %lx. end %lx \n", (*it).first, (*it).first+(*it).second.lengthAdr);
				if ( (size_t)(*it).first + (*it).second.lengthAdr >= pointer) {
					 
					 //toReturn = strdup((*(*it).second.func).getName().data());
					 toReturn = (*(*it).second.func).getName().data();
					 break;
				}
			}/*else {
				toReturn = "not found";
				break;
			}*/
	
		}
		//printf("exiting getfuncinfo ");
		return toReturn;
	}


		/*

		char* getFunctionInfo(size_t pointer) {
			return "Debug Version of function";

		}
		*/

		

		//return jl_jit_events->findFunc(pointer).getName().convertFromString();


	







