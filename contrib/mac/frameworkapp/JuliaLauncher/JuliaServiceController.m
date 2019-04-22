// This file is a part of Julia. License is MIT: https://julialang.org/license

#import "JuliaServiceController.h"
#import "AppDelegate.h"

@implementation JuliaServiceController

- (instancetype)initWithJuliaVariant:(JuliaVariant *)jv {
  self = [super init];
  if (!self) {
    return nil;
  }
  _juliaVariant = jv;
  return self;
}

/// Execute the Julia program on the pasteboard in a new julia instance.
- (void)execProgram:(NSPasteboard *)pboard
           userData:(NSString *)userData
              error:(NSString **)error {
  [self execProgram:pboard result:nil userData:userData error:error];
}

/// Evaluate the Julia program on the pasteboard and send back the result.
- (void)evalProgram:(NSPasteboard *)pboard
           userData:(NSString *)userData
              error:(NSString **)error {
  NSFileHandle *stdoutHandle = nil;

  [self execProgram:pboard result:&stdoutHandle userData:userData error:error];

  if (stdoutHandle == nil) {
    return;
  }

  NSData *stdoutData = [stdoutHandle readDataToEndOfFile];
  NSString *stdoutResult = [[NSString alloc] initWithData:stdoutData
                                                 encoding:NSUTF8StringEncoding];

  [pboard clearContents];
  [pboard writeObjects:@[ stdoutResult ]];
}

- (void)execProgram:(NSPasteboard *)pboard
             result:(NSFileHandle **)stdoutHandle
           userData:(NSString *)userData
              error:(NSString **)error {
  NSArray *classes = @ [[NSURL class], [NSString class]];
  NSDictionary *options = @{
    NSPasteboardURLReadingFileURLsOnlyKey : @(YES),
    NSPasteboardURLReadingContentsConformToTypesKey :
        @[ @"org.julialang.julia.text" ]
  };

  NSArray *pbObjects = [pboard readObjectsForClasses:classes options:options];
  if (pbObjects == nil) {
    NSLog(@"Error retrieving the requested items from service pasteboard.");
    return;
  }

  if (pbObjects.count == 0) {
    NSLog(@"No retrievable URLs or strings on service pasteboard. %@",
          pboard.types);
    return;
  }

  for (id pbItem in pbObjects) {
    // Create a temporary directory to work in.
    NSURL *workingDirectoryURL = nil;

    // Write the program to a file.
    NSURL *programURL = nil;

    BOOL alertUser = stdoutHandle == nil;
    BOOL cleanup = NO;
    NSURL *givenFileURL = nil;

    if ([pbItem isKindOfClass:[NSURL class]]) {
      if (![pbItem isFileURL]) {
        NSLog(@"Cannot exec Julia program because the URL is not a file: %@",
              pbItem);
        continue;
      }
      programURL = pbItem;
      programURL = programURL.filePathURL;
      givenFileURL = programURL;
      workingDirectoryURL = [programURL URLByDeletingLastPathComponent];
      NSLog(@"Exec Julia program %@", programURL);
    } else if ([pbItem isKindOfClass:[NSString class]]) {
      workingDirectoryURL = [NSURL fileURLWithPath:NSTemporaryDirectory()
                                       isDirectory:YES];
      workingDirectoryURL = [workingDirectoryURL
          URLByAppendingPathComponent:[[NSProcessInfo processInfo]
                                          globallyUniqueString]
                          isDirectory:true];
      NSError *error = nil;
      if (![[NSFileManager defaultManager]
                     createDirectoryAtURL:workingDirectoryURL
              withIntermediateDirectories:YES
                               attributes:nil
                                    error:&error]) {
        NSLog(@"Failed creating temporary directory %@\n%@",
              workingDirectoryURL, error);
        continue;
      }

      NSString *program = pbItem;
      NSLog(@"Exec Julia program:\n%@", program);

      NSString *temporaryFilename =
          [[NSProcessInfo processInfo] globallyUniqueString];
      programURL =
          [workingDirectoryURL URLByAppendingPathComponent:temporaryFilename
                                               isDirectory:NO];

      [[program dataUsingEncoding:NSUTF8StringEncoding] writeToURL:programURL
                                                        atomically:NO];

      cleanup = YES;
    } else {
      NSLog(@"Unexpected item type %@", [pbItem class]);
      continue;
    }

    NSTask *t = [[NSTask alloc] init];
    if (@available(macOS 10.13, *)) {
      t.executableURL = _juliaVariant.juliaexe;
    } else {
      t.launchPath = _juliaVariant.juliaexe.path;
    }
    t.arguments = @[ @"--", programURL.path ];
    if (@available(macOS 10.13, *)) {
      t.currentDirectoryURL = workingDirectoryURL;
    } else {
      t.currentDirectoryPath = workingDirectoryURL.path;
    }
    t.terminationHandler = ^(NSTask *_Nonnull t) {
      if (alertUser) {
        dispatch_async(dispatch_get_main_queue(), ^{
          NSAlert *a = [[NSAlert alloc] init];
          a.alertStyle = NSAlertStyleInformational;
          a.messageText =
              NSLocalizedString(@"Julia finished running requested program.", );
          NSMutableString *informativeText = [NSMutableString
              stringWithFormat:@"%@%@Exit status = %d",
                               givenFileURL ? givenFileURL.path : @"",
                               givenFileURL ? @"\n" : @"", t.terminationStatus];
          if (t.terminationReason == NSTaskTerminationReasonUncaughtSignal) {
            [informativeText appendString:@" (uncaught signal)"];
          }
          a.informativeText = informativeText;
          [a runModal];
        });
        if (cleanup) {
          [[NSFileManager defaultManager] removeItemAtURL:workingDirectoryURL
                                                    error:nil];
        }
      }
    };
    if (stdoutHandle) {
      NSPipe *stdoutPipe = [NSPipe pipe];
      t.standardOutput = stdoutPipe;
      *stdoutHandle = stdoutPipe.fileHandleForReading;
    }
    @try {
      [t launch];
    } @catch (NSException *exception) {
      NSLog(@"Failed running service for program. %@", exception);
    }
    break;
  }
}

@end
