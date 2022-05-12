// This file is a part of Julia. License is MIT: https://julialang.org/license

@import Foundation;

@protocol TaskProtocol
/// Launch the task and upon termination receive the exit status.
- (void)launch:(void (^_Nullable)(int status))onTermination;
/// Terminate (SIGTERM) the task.
- (void)terminate;
@end

@protocol ExecSandboxProtocol
/**
 Evaluate a Julia program with a Julia executable.

 @param juliaProgram Julia source code to be evaluated.
 @param executableBookmark NSURL file bookmark for the julia executable to run.
 @param args Arguments to pass to julia.
 @param reply Async result with task and standard in, out, and error. An error
 occurred if task is nil.
 */
- (void)eval:(NSString *_Nonnull)juliaProgram
    withJulia:(NSData *_Nonnull)executableBookmark
    arguments:(NSArray<NSString *> *_Nullable)args
         task:(void (^_Nonnull)(id<TaskProtocol> _Nullable task,
                                NSFileHandle *_Nullable stdIn,
                                NSFileHandle *_Nullable stdOut,
                                NSFileHandle *_Nullable stdErr))reply;
@end

NSXPCInterface *_Nonnull CreateExecSandboxXPCInterface(void);
