#include <iostream>

#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclGroup.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Parse/Parser.h"

using namespace std;
using namespace clang;

class PrintFunctionsConsumer : public ASTConsumer {
public:
    virtual bool HandleTopLevelDecl(DeclGroupRef DG) {
        for (DeclGroupRef::iterator i = DG.begin(), e = DG.end(); i != e; ++i) {
            const Decl *D = *i;
            const FunctionDecl *FD = dyn_cast<FunctionDecl>(D);
            //D->dump();
            //cout << '\n';
            //return true;
            if (!FD || !FD->hasPrototype() || !FD->isExternC() || !FD->isGlobal() || FD->getBuiltinID() != 0) return true;
            cout << FD->getResultType().getAsString() << " ";
            cout << FD->getNameAsString() << "(";
            bool printComma = false;
            FunctionDecl::param_const_iterator I = FD->param_begin(),
                                               E = FD->param_end();
            while (I != E) {
                ParmVarDecl *PVD = *I++;
                if (printComma) cout << ", ";
                cout << PVD->getOriginalType().getAsString();
                printComma = true;
            }
            cout << ");\n";
        }
        return true;
    }
};

int main()
{
    CompilerInstance ci;
    ci.createDiagnostics(0,NULL);

    TargetOptions to;
    to.Triple = llvm::sys::getDefaultTargetTriple();
    TargetInfo *pti = TargetInfo::CreateTargetInfo(ci.getDiagnostics(), to);
    ci.setTarget(pti);

    ci.getHeaderSearchOpts().AddPath(
        StringRef("../usr/lib/clang/3.0/include"), frontend::Angled, false, false, false
    );
    ci.getHeaderSearchOpts().AddPath(
        StringRef("support"), frontend::Quoted, true, false, false
    );

    ci.createFileManager();
    ci.createSourceManager(ci.getFileManager());
    ci.createPreprocessor();
    PrintFunctionsConsumer *astConsumer = new PrintFunctionsConsumer();
    ci.setASTConsumer(astConsumer);

    ci.createASTContext();
    const FileEntry *pFile = ci.getFileManager().getFile("julia.h");
    ci.getSourceManager().createMainFileID(pFile);
    ci.getDiagnosticClient().BeginSourceFile(ci.getLangOpts(), &ci.getPreprocessor());
    clang::ParseAST(ci.getPreprocessor(), astConsumer, ci.getASTContext());
    ci.getDiagnosticClient().EndSourceFile();

    return 0;
}
