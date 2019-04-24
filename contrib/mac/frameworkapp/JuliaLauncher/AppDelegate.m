// This file is a part of Julia. License is MIT: https://julialang.org/license

@import AppKit;
#import "AppDelegate.h"
#import "JuliaVariant.h"

/// Terminal's bundle ID.
NSString static const *const terminalBundleID = @"com.apple.Terminal";

@interface AppDelegate () {
  JuliaVariantController *_jvman;
}
- (void)execBestJulia;
@end

@implementation AppDelegate

- (instancetype)init {
  self = [super init];
  if (!self) {
    return nil;
  }
  _jvman = [[JuliaVariantController alloc] init];
  return self;
}

- (void)execBestJulia {
  JuliaVariant *jv = _jvman.defaultJuliaVariant;
  if (!jv) {
    jv = _jvman.latestKnownTaggedJulia;
  }
  if (!jv) {
    return;
  }

  // TODO: Verify julia is "safe" to run. Check code signature? Ask user?

  NSURL *juliaexe = jv.juliaexe;
  if (juliaexe) {
    [AppDelegate execJuliaInTerminal:juliaexe
                                exec:YES
                         interactive:YES
                       fileOrProgram:nil];
  }
}

// MARK: Responder -

- (IBAction)newDocument:(id)sender {
  if (!_jvman.defaultJuliaVariant) {
    [_jvman askForDefault];
    return;
  }

  [self execBestJulia];
}

- (IBAction)showHelp:(id)sender {
  JuliaVariant *jv = _jvman.defaultJuliaVariant;
  if (!jv) {
    [NSApp showHelp:sender];
    return;
  }

  NSURL *helpURL =
      [[[[jv.bundle.bundleURL URLByAppendingPathComponent:@"Documentation"
                                              isDirectory:true]
          URLByAppendingPathComponent:@"html"
                          isDirectory:true] URLByAppendingPathComponent:@"en"
                                                            isDirectory:true]
          URLByAppendingPathComponent:@"index.html"
                          isDirectory:false];

  [[NSWorkspace sharedWorkspace] openURL:helpURL];
}

// MARK: App Delegate -

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
  [NSApp disableRelaunchOnLogin];
  [_jvman readFromDefaults];
  [self execBestJulia];
  [_jvman findJuliaWithSpotlight];
}

- (BOOL)applicationShouldHandleReopen:(NSApplication *)sender
                    hasVisibleWindows:(BOOL)flag {
  [self execBestJulia];
  return NO;
}

- (IBAction)showVersionManager:(id)sender {
  [_jvman.variantsWindow makeKeyAndOrderFront:nil];
}

+ (void)execJuliaInTerminal:(NSURL *_Nonnull)julia
                       exec:(BOOL)execFlag
                interactive:(BOOL)interactiveFlag
              fileOrProgram:(id _Nullable)fileOrProgram {
  OSStatus s;
  NSAlert *a = [[NSAlert alloc] init];
  a.alertStyle = NSAlertStyleCritical;
  a.messageText = NSLocalizedString(@"Cannot run the Julia REPL.", );

  // Create the AE descriptor for the bundle ID of Terminal.
  NSData *terminalBundleIDData =
      [terminalBundleID dataUsingEncoding:NSUTF8StringEncoding];
  NSAppleEventDescriptor *aedTarget = [NSAppleEventDescriptor
      descriptorWithDescriptorType:typeApplicationBundleID
                              data:terminalBundleIDData];

  // Command: activate
  NSAppleEventDescriptor *aevActivate =
      [NSAppleEventDescriptor appleEventWithEventClass:kAEMiscStandards
                                               eventID:kAEActivate
                                      targetDescriptor:aedTarget
                                              returnID:kAutoGenerateReturnID
                                         transactionID:kAnyTransactionID];

  // Command: do shell script
  NSAppleEventDescriptor *aevShellCmd =
      [NSAppleEventDescriptor appleEventWithEventClass:kAECoreSuite
                                               eventID:kAEDoScript
                                      targetDescriptor:aedTarget
                                              returnID:kAutoGenerateReturnID
                                         transactionID:kAnyTransactionID];

  // Set shell script as direct parameter.
  char const *juliaFSRepCstr;
  if (@available(macOS 10.9, *)) {
    juliaFSRepCstr = julia.fileSystemRepresentation;
  } else {
    juliaFSRepCstr = julia.path.fileSystemRepresentation;
  }

  // Add ';' at start of command to mitigate a race condition.
  // Between the time the shell opens and the "exec ..." command is inserted,
  // it's possible for stray key events to insert leading to the command being
  // "asdf;exec ..." (for example). The semicolon mitigates the stray key
  // presses.
  NSMutableString *execJuliaCmd = [NSMutableString
      stringWithFormat:@";%s'%s'%s", execFlag ? "exec " : "", juliaFSRepCstr,
                       interactiveFlag ? " -i" : ""];

  if (fileOrProgram) {
    char const *programFSRepCstr;
    if ([fileOrProgram isKindOfClass:[NSURL class]]) {
      NSURL *programFileURL = fileOrProgram;
      if (@available(macOS 10.9, *)) {
        programFSRepCstr = programFileURL.fileSystemRepresentation;
      } else {
        programFSRepCstr = programFileURL.path.fileSystemRepresentation;
      }
    } else if ([fileOrProgram isKindOfClass:[NSString class]]) {
      NSURL *temporaryDirectoryURL =
          [NSURL fileURLWithPath:NSTemporaryDirectory() isDirectory:YES];
      temporaryDirectoryURL = [temporaryDirectoryURL
          URLByAppendingPathComponent:[[NSProcessInfo processInfo]
                                          globallyUniqueString]
                          isDirectory:true];
      NSError *error = nil;
      if (![[NSFileManager defaultManager]
                     createDirectoryAtURL:temporaryDirectoryURL
              withIntermediateDirectories:YES
                               attributes:nil
                                    error:&error]) {
        NSLog(@"Failed creating temporary directory %@\n%@",
              temporaryDirectoryURL, error);
        return;
      }

      NSString *temporaryFilename =
          [[NSProcessInfo processInfo] globallyUniqueString];
      NSURL *temporaryFileURL =
          [temporaryDirectoryURL URLByAppendingPathComponent:temporaryFilename
                                                 isDirectory:NO];

      [[fileOrProgram dataUsingEncoding:NSUTF8StringEncoding]
          writeToURL:temporaryFileURL
          atomically:NO];

      if (@available(macOS 10.9, *)) {
        programFSRepCstr = temporaryFileURL.fileSystemRepresentation;
      } else {
        programFSRepCstr = temporaryFileURL.path.fileSystemRepresentation;
      }
    } else {
      NSLog(@"Unexpected type '%@' for fileOrProgram.", [fileOrProgram class]);
      return;
    }
    [execJuliaCmd appendFormat:@" -- '%s'", programFSRepCstr];
  }

  [execJuliaCmd appendString:@";"];

  NSAppleEventDescriptor *shellScript =
      [NSAppleEventDescriptor descriptorWithString:execJuliaCmd];
  [aevShellCmd setParamDescriptor:shellScript forKeyword:keyDirectObject];

  if (@available(macOS 10.14, *)) {
    bool retry = false;

    do {
      s = AEDeterminePermissionToAutomateTarget(
          aedTarget.aeDesc, aevShellCmd.eventClass, aevShellCmd.eventID, TRUE);
      retry = s == procNotFound && !retry && [AppDelegate launchTerminalApp];
    } while (retry);

    if (s == errAEEventNotPermitted) {
      a.informativeText = NSLocalizedString(
          @"The system prevented running the Julia REPL in the Terminal.", );
    } else if (s == errAETargetAddressNotPermitted) {
      a.informativeText = NSLocalizedString(
          @"The system prevented sending AppleEvents to Terminal.", );
    } else if (s == procNotFound) {
      a.informativeText = NSLocalizedString(
          @"Terminal is not running and could not be launched.", );
    }

    if (s != noErr) {
      [[NSRunLoop mainRunLoop] performBlock:^{
        [a runModal];
      }];
      NSLog(@"AEDeterminePermissionToAutomateTarget failed: %@",
            [[NSError errorWithDomain:NSOSStatusErrorDomain code:s
                             userInfo:nil] localizedDescription]);
      return;
    }
  }

  s = AESendMessage(aevShellCmd.aeDesc, NULL, kAENoReply | kAECanInteract,
                    kAEDefaultTimeout);

  if (s != noErr) {
    a.informativeText = NSLocalizedString(@"\"do script\" command failed.", );
    dispatch_async(dispatch_get_main_queue(), ^{
      [a runModal];
    });
    NSLog(@"\"do script\" failed: %@",
          [[NSError errorWithDomain:NSOSStatusErrorDomain code:s
                           userInfo:nil] localizedDescription]);
    return;
  }

  s = AESendMessage(aevActivate.aeDesc, NULL, kAENoReply | kAECanInteract,
                    kAEDefaultTimeout);

  if (s != noErr) {
    a.informativeText = NSLocalizedString(@"\"activate\" command failed.", );
    dispatch_async(dispatch_get_main_queue(), ^{
      [a runModal];
    });
    NSLog(@"\"activate\" failed: %@",
          [[NSError errorWithDomain:NSOSStatusErrorDomain code:s
                           userInfo:nil] localizedDescription]);
    return;
  }
}

+ (BOOL)launchTerminalApp {
  // Launch Terminal.app.  Use newer or older API depending on OS version.

  // Send hint to Terminal to not open a new window when opened.
  NSAppleEventDescriptor *aedLaunchedAsServiceItem = [NSAppleEventDescriptor
      descriptorWithEnumCode:keyAELaunchedAsServiceItem];

  // Common launch URL spec for either old or new API.
  LSLaunchURLSpec lspec = {.launchFlags = kLSLaunchDontSwitch,
                           .passThruParams = aedLaunchedAsServiceItem.aeDesc};

  if (@available(macOS 10.10, *)) {
    NSArray<NSURL *> *terminalURLs =
        (__bridge NSArray<NSURL *> *)LSCopyApplicationURLsForBundleIdentifier(
            (__bridge CFStringRef)terminalBundleID, NULL);
    for (NSURL *terminalURL in terminalURLs) {
      lspec.appURL = (__bridge CFURLRef)terminalURL;
      OSStatus s = LSOpenFromURLSpec(&lspec, NULL);
      if (s == noErr) {
        return YES;
      } else {
        NSLog(@"LSOpenFromURLSpec failed for %@\n%@", terminalURL,
              [[NSError errorWithDomain:NSOSStatusErrorDomain
                                   code:s
                               userInfo:nil] localizedDescription]);
      }
    }
  } else {
    CFURLRef terminalURL = NULL;
    LSFindApplicationForInfo(kLSUnknownCreator,
                             (__bridge CFStringRef)terminalBundleID, NULL, NULL,
                             &terminalURL);
    if (terminalURL != NULL) {
      lspec.appURL = terminalURL;
      OSStatus s = LSOpenFromURLSpec(&lspec, NULL);
      CFRelease(terminalURL);
      if (s == noErr) {
        return YES;
      } else {
        NSLog(@"LSOpenFromURLSpec failed for %@\n%@", terminalURL,
              [[NSError errorWithDomain:NSOSStatusErrorDomain
                                   code:s
                               userInfo:nil] localizedDescription]);
      }
    }
  }

  return NO;
}

@end
