// This file is a part of Julia. License is MIT: https://julialang.org/license

@import AppKit;

@interface AppDelegate : NSObject <NSApplicationDelegate>
+ (void)execJuliaInTerminal:(NSURL *_Nonnull)julia
                       exec:(BOOL)execFlag
                interactive:(BOOL)interactiveFlag
              fileOrProgram:(id _Nullable)fileOrProgram;
@end
