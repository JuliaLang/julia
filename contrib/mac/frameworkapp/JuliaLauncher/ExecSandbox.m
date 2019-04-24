// This file is a part of Julia. License is MIT: https://julialang.org/license

#import "ExecSandbox.h"

@interface ExecSandboxController () {
  NSXPCConnection *_Nonnull _execSandboxCnx;
}
@end

@implementation ExecSandboxController

- (instancetype)init {
  self = [super init];
  if (self == nil)
    return nil;
  _execSandboxCnx = [[NSXPCConnection alloc]
      initWithServiceName:@"org.julialang.JuliaLauncher.ExecSandbox"];
  _execSandboxCnx.remoteObjectInterface = CreateExecSandboxXPCInterface();
  [_execSandboxCnx resume];
  return self;
}

- (id<ExecSandboxProtocol>)remoteObjectProxyWithErrorHandler:
    (void (^_Nonnull)(NSError *_Nullable error))handler {
  return [_execSandboxCnx remoteObjectProxyWithErrorHandler:handler];
}

+ (ExecSandboxController *)sharedController {
  static ExecSandboxController *s = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    s = [[ExecSandboxController alloc] init];
  });
  return s;
}

@end
