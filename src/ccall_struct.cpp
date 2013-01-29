//for clang to llvm use CodeGenTypes::ConvertType()
//for llvm to clang, manual?
//  via BuiltinType(Kind) WRONG
//  Kind declared in clang/AST/BuiltinTypes.def
//  unsigned: unsigned char, unsigned short, unsigned int, unsigned long long, unsigned long, unsigned __uint128_t
//  signed: same
//  floats: float, double, long double
//  void
//  
//  All created / accessed through ASTContext

ASTContext::ASTContext  (   LangOptions &   LOpts,
SourceManager &     SM, 
const TargetInfo *  t,  
IdentifierTable &   idents,
SelectorTable &     sels,
Builtin::Context &  builtins,
unsigned    size_reserve,
bool    DelayInitialization = false 
)   
//  (Context, SelectorTable, IdentifierTable, LangOptions are easy)
//  (TargetInfo is a big pain)
//  (SourceManager looks easy enough)
//
//  in CGCall.h
//    static CGFunctionInfo *create(unsigned llvmCC,
//                                  const FunctionType::ExtInfo &extInfo,
//                                  CanQualType resultType,
//                                  ArrayRef<CanQualType> argTypes,
//                                  RequiredArgs required);
//  in ABIInfo.h
//    virtual void computeInfo(CodeGen::CGFunctionInfo &FI) const = 0;
// 
//  called from
const CGFunctionInfo &
CodeGenTypes::arrangeLLVMFunctionInfo(CanQualType resultType,
                                      ArrayRef<CanQualType> argTypes,
                                      FunctionType::ExtInfo info,
                                      RequiredArgs required) {
// final stage
llvm::FunctionType *
CodeGenTypes::GetFunctionType(const CGFunctionInfo &FI) {
//  then
RValue CodeGenFunction::EmitCall(const CGFunctionInfo &CallInfo,
                                 llvm::Value *Callee,
                                 ReturnValueSlot ReturnValue,
                                 const CallArgList &CallArgs,
                                 const Decl *TargetDecl,
                                 llvm::Instruction **callOrInvoke) {
// (RValue and ReturnValueSlot, CallArgList are easy to work with)  
// (TargetDecl can just be null -- it is info from Callee target to copy to call)
// Fn will need attribute NoUnwind (or it gets turned into an invoke)
//
//
// for structs, need
// http://clang.llvm.org/doxygen/classclang_1_1FieldDecl.html for each field
// then create a http://clang.llvm.org/doxygen/classclang_1_1RecordDecl.html#a7351c179c5708122c856eb9ffb59c634// as a http://clang.llvm.org/doxygen/classclang_1_1TagDecl.html
// and setTemplateParameterListsInfo to a
// http://clang.llvm.org/doxygen/classclang_1_1TemplateParameterList.html
// constructed of the fields
// then can http://clang.llvm.org/doxygen/classclang_1_1ASTContext.html#a2f2e06312812efc11ce91abe0695ba75
// and http://clang.llvm.org/doxygen/classclang_1_1ASTContext.html#a7e70bb7eebf01548a0cab752e390ba37
// and that's it!
