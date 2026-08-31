#include "JuliaDialect.h"

#define GET_INCLUDES
#include "JuliaDialect.cpp.inc"

#define GET_DIALECT_DEFS
#include "JuliaDialect.cpp.inc"

#include <llvm/ADT/DenseMap.h>
#include <llvm-dialects/Dialect/Dialect.h>

#include <mutex>

using namespace llvm;

namespace julia {

// Per-context refcounted registry backing ScopedDialects. The entry is
// erased when the last attachment goes away, so a context address that gets
// reused by a later LLVMContext starts from a clean slate.
namespace {
struct DialectAttachment {
    std::unique_ptr<llvm_dialects::DialectContext> dc;
    unsigned refcount = 0;
};
std::mutex attachmentsLock;
DenseMap<LLVMContext *, DialectAttachment> attachments;
} // anonymous namespace

ScopedDialects::ScopedDialects(LLVMContext &ctx) : ctx(&ctx)
{
    std::lock_guard<std::mutex> lock(attachmentsLock);
    auto &attachment = attachments[&ctx];
    if (attachment.refcount++ == 0)
        attachment.dc = llvm_dialects::DialectContext::make<JuliaDialect>(ctx);
}

ScopedDialects::~ScopedDialects()
{
    if (!ctx) // moved from
        return;
    std::lock_guard<std::mutex> lock(attachmentsLock);
    auto it = attachments.find(ctx);
    assert(it != attachments.end() && it->second.refcount > 0);
    if (--it->second.refcount == 0)
        attachments.erase(it);
}

} // namespace julia
