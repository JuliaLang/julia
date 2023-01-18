// This file is a part of Julia. License is MIT: https://julialang.org/license

@import Foundation;
#import "ExecSandbox.h"

@interface ServiceDelegate : NSObject <NSXPCListenerDelegate>
@end

@implementation ServiceDelegate

- (BOOL)listener:(NSXPCListener *)listener
    shouldAcceptNewConnection:(NSXPCConnection *)newConnection {
  newConnection.exportedInterface = CreateExecSandboxXPCInterface();
  ExecSandbox *exportedObject = [[ExecSandbox alloc] init];
  newConnection.exportedObject = exportedObject;
  [newConnection resume];
  return YES;
}

@end

int main(int argc, const char *argv[]) {
  ServiceDelegate *delegate = [[ServiceDelegate alloc] init];
  NSXPCListener *listener = [NSXPCListener serviceListener];
  listener.delegate = delegate;
  [listener resume];
  return 0;
}
