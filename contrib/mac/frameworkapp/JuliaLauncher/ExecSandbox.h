// This file is a part of Julia. License is MIT: https://julialang.org/license

@import Foundation;
#import "ExecSandboxProtocol.h"

/// Controller for an XPC connection to the ExecSandbox service.
///
/// The ExecSandbox service allows Julia code to be run within a restricted App
/// Sandbox environment.
@interface ExecSandboxController : NSObject
- (id<ExecSandboxProtocol> _Nonnull)remoteObjectProxyWithErrorHandler:
    (void (^_Nonnull)(NSError *_Nullable error))handler;
+ (ExecSandboxController *_Nonnull)sharedController;
@end
