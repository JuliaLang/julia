// This file is a part of Julia. License is MIT: https://julialang.org/license

#import "ExecSandbox.h"

@class JuliaTask;

@interface ExecSandbox () {
  NSMutableArray<JuliaTask *> *_Nonnull _tasks;
}
- (void)taskTerminated:(JuliaTask *_Nonnull)jt;
@end

@interface JuliaTask : NSObject <TaskProtocol> {
  ExecSandbox *__weak _delegate;
  dispatch_block_t _Nullable _onCleanup;
  NSTask *_Nonnull _task;
}
@end

@implementation JuliaTask

- (instancetype)initWithTask:(NSTask *_Nonnull)task
                    delegate:(ExecSandbox *)d
                     cleanup:(dispatch_block_t)onCleanup {
  self = [super init];
  if (self == nil) {
    return nil;
  }
  _delegate = d;
  _onCleanup = onCleanup;
  _task = task;
  return self;
}

- (void)launch:(void (^_Nullable)(int status))onTermination {
  dispatch_block_t onCleanup = _onCleanup;
  JuliaTask __weak *weakSelf = self;
  _task.terminationHandler = ^(NSTask *_Nonnull t) {
    if (onTermination != nil) {
      onTermination(t.terminationStatus);
    }
    if (onCleanup != nil) {
      onCleanup();
    }
    JuliaTask *strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf->_delegate taskTerminated:strongSelf];
    }
  };
  @try {
    [_task launch];
  } @catch (NSException *exception) {
    NSLog(@"NSTask launch exception: %@", exception);
  }
}

- (void)terminate {
  @try {
    [_task terminate];
  } @catch (NSException *exception) {
    NSLog(@"NSTask terminate exception: %@", exception);
  }
}

@end

@implementation ExecSandbox

- (instancetype)init {
  self = [super init];
  if (self == nil) {
    return nil;
  }
  _tasks = [[NSMutableArray alloc] init];
  return self;
}

- (void)eval:(NSString *)p
    withJulia:(NSData *)executableBookmark
    arguments:(NSArray<NSString *> *)baseArgs
         task:(void (^)(id<TaskProtocol> task, NSFileHandle *stdIn,
                        NSFileHandle *stdOut, NSFileHandle *stdErr))reply {

  NSURL *executableURL =
      [NSURL URLByResolvingBookmarkData:executableBookmark
                                options:NSURLBookmarkResolutionWithoutUI
                          relativeToURL:nil
                    bookmarkDataIsStale:nil
                                  error:nil];
  if (executableURL == nil) {
    reply(nil, nil, nil, nil);
    return;
  }

  for (NSString *arg in baseArgs) {
    if ([arg isEqual:@"--"]) {
      reply(nil, nil, nil, nil);
      return;
    }
  }

  NSURL *temporaryDirectoryURL = [NSURL fileURLWithPath:NSTemporaryDirectory()
                                            isDirectory:YES];
  NSString *temporaryFilename =
      [[NSProcessInfo processInfo] globallyUniqueString];
  NSURL *temporaryFileURL =
      [temporaryDirectoryURL URLByAppendingPathComponent:temporaryFilename
                                             isDirectory:NO];

  [[p dataUsingEncoding:NSUTF8StringEncoding] writeToURL:temporaryFileURL
                                              atomically:NO];

  NSMutableArray<NSString *> *args = [[NSMutableArray alloc] init];
  [args addObjectsFromArray:baseArgs];
  [args addObjectsFromArray:@[ @"--", temporaryFileURL.path ]];

  NSPipe *stdIn = [NSPipe pipe], *stdOut = [NSPipe pipe],
         *stdErr = [NSPipe pipe];

  NSTask *t = [[NSTask alloc] init];
  if (@available(macOS 10.13, *)) {
    t.executableURL = executableURL;
  } else {
    t.launchPath = executableURL.path;
  }
  t.arguments = args;
  t.standardInput = stdIn;
  t.standardOutput = stdOut;
  t.standardError = stdErr;

  JuliaTask *jt =
      [[JuliaTask alloc] initWithTask:t
                             delegate:self
                              cleanup:^() {
                                [[NSFileManager defaultManager]
                                    removeItemAtURL:temporaryDirectoryURL
                                              error:nil];
                              }];
  [_tasks addObject:jt];

  reply(jt, stdIn.fileHandleForWriting, stdOut.fileHandleForReading,
        stdErr.fileHandleForReading);
}

- (void)taskTerminated:(JuliaTask *_Nonnull)jt {
  [_tasks removeObject:jt];
}

@end
