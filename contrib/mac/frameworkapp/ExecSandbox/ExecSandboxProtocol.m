// This file is a part of Julia. License is MIT: https://julialang.org/license

#import "ExecSandboxProtocol.h"

NSXPCInterface *CreateExecSandboxXPCInterface(void) {
  NSXPCInterface *i =
      [NSXPCInterface interfaceWithProtocol:@protocol(ExecSandboxProtocol)];
  /// Reply sends a task proxy:
  [i setInterface:[NSXPCInterface interfaceWithProtocol:@protocol(TaskProtocol)]
        forSelector:@selector(eval:withJulia:arguments:task:)
      argumentIndex:0
            ofReply:true];
  return i;
}
