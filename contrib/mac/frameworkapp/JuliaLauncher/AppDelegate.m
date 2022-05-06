// This file is a part of Julia. License is MIT: https://julialang.org/license

@import AppKit;
#import "AppDelegate.h"
#import "ExecSandboxProtocol.h"

/// Terminal's bundle ID.
NSString static const *const terminalBundleID = @"com.apple.Terminal";

static bool launchTerminalApp(void);
static void execJuliaInTerminal(NSURL *_Nonnull julia);

/// Controller for an XPC connection to the ExecSandbox service.
///
/// The ExecSandbox service allows Julia code to be run within a restricted App
/// Sandbox environment.
@interface ExecSandboxController : NSObject {
  NSXPCConnection *_Nonnull _execSandboxCnx;
}
- (id<ExecSandboxProtocol> _Nonnull)remoteObjectProxyWithErrorHandler:
    (void (^_Nonnull)(NSError *_Nullable error))handler;
+ (ExecSandboxController *_Nonnull)sharedController;
@end

@implementation ExecSandboxController

- (instancetype)init {
  self = [super init];
  if (self == nil)
    return nil;
  _execSandboxCnx = [[NSXPCConnection alloc]
      initWithServiceName:@"org.julialang.ExecSandbox"];
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

/// Location of an installed variant of Julia (frameowrk or nix hier).
@interface JuliaVariant : NSObject
@property(readonly, nullable) NSBundle *bundle;
@property(readonly, nonnull) NSURL *juliaexe;
@property(readonly, nullable) NSString *version;
@property(readonly) BOOL updatingVersion;
- (instancetype)initWithJulia:(NSURL *_Nonnull)exe
                       bundle:(NSBundle *_Nullable)b;
/// (major,minor,patch) components parsed from version.
@property(readonly, nullable) NSArray<NSNumber *> *versionComponents;
@end

@implementation JuliaVariant

- (instancetype)initWithJulia:(NSURL *)exe bundle:(NSBundle *)b {
  self = [super init];
  if (!self) {
    return nil;
  }
  NSAssert(exe != nil, @"juliaexe cannot be nil.");
  _juliaexe = exe;
  _bundle = b;
  _version = nil;
  if (_bundle == nil) {
    // Try to locate the framework bundle.
    NSURL *frameworkURL =
        [[exe URLByDeletingLastPathComponent] URLByDeletingLastPathComponent];
    NSBundle *bundle = [NSBundle bundleWithURL:frameworkURL];
    if (bundle &&
        [[bundle bundleIdentifier] isEqual:@"org.julialang.julia.lib"]) {
      _bundle = bundle;
    }
  }
  if (_bundle) {
    // Extract version from framework bundle.
    _version = _bundle.infoDictionary[(NSString *)kCFBundleVersionKey];
  } else {
    // Exec the julia and have it tell us its version.

    NSData *juliaexeBookmark = [_juliaexe bookmarkDataWithOptions:0
                                   includingResourceValuesForKeys:nil
                                                    relativeToURL:nil
                                                            error:nil];

    _updatingVersion = YES;

    id<ExecSandboxProtocol> remote = [[ExecSandboxController sharedController]
        remoteObjectProxyWithErrorHandler:^(NSError *error) {
          [self willChangeValueForKey:@"updatingVersion"];
          self->_updatingVersion = NO;
          [self didChangeValueForKey:@"updatingVersion"];
        }];

    [remote eval:@"print(\"$(Base.VERSION.major).$(Base.VERSION.minor).$(Base."
                 @"VERSION.patch)\")"
        withJulia:juliaexeBookmark
        arguments:nil
             task:^(id<TaskProtocol> task, NSFileHandle *stdIn,
                    NSFileHandle *stdOut, NSFileHandle *stdErr) {
               [task launch:^(int status) {
                 NSString *vout =
                     [[NSString alloc] initWithData:[stdOut readDataToEndOfFile]
                                           encoding:NSUTF8StringEncoding];
                 if (status == 0 && vout) {
                   [self willChangeValueForKey:@"version"];
                   [self willChangeValueForKey:@"updatingVersion"];
                   self->_version = vout;
                   self->_updatingVersion = NO;
                   [self didChangeValueForKey:@"updatingVersion"];
                   [self didChangeValueForKey:@"version"];

                 } else {
                   [self willChangeValueForKey:@"updatingVersion"];
                   self->_updatingVersion = NO;
                   [self didChangeValueForKey:@"updatingVersion"];
                 }
               }];
             }];
    NSLog(@"Getting version by execing %@", exe);
  }
  return self;
}

- (NSArray<NSNumber *> *)versionComponents {
  NSNumber *compsi[3];
  NSArray<NSString *> *c = [self.version componentsSeparatedByString:@"."];
  if (c.count < 3) {
    return nil;
  }
  NSInteger i = 0;
  for (NSString *vn in c) {
    compsi[i++] = @(vn.integerValue);
  }
  if ([compsi[0] isEqual:@(0)] && [compsi[1] isEqual:@(0)] &&
      [compsi[2] isEqual:@(0)]) {
    return nil;
  }
  return [NSArray arrayWithObjects:compsi count:3];
}

@end

@interface AppDelegate () {
  NSMetadataQuery *_Nullable _mdq;
}
@property NSMutableDictionary<NSURL *, JuliaVariant *> *_Nonnull juliaVariants;
@property JuliaVariant *_Nullable latestKnownTaggedJulia;
@end

@implementation AppDelegate

- (instancetype)init {
  self = [super init];
  if (!self) {
    return nil;
  }
  _juliaVariants = [[NSMutableDictionary alloc] init];
  return self;
}

- (void)dealloc {
  [self stopFindJuliaWithSpotlight];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
  [self findJuliaWithSpotlight];
}

- (BOOL)applicationShouldHandleReopen:(NSApplication *)sender
                    hasVisibleWindows:(BOOL)flag {
  if (!_mdq.gathering) {
    NSURL *juliaexe = self.latestKnownTaggedJulia.juliaexe;
    if (juliaexe) {
      execJuliaInTerminal(juliaexe);
    }
  }
  return NO;
}

- (void)addJuliaVariant:(JuliaVariant *)jv {
  if ([self.juliaVariants objectForKey:jv.juliaexe]) {
    // Don't overwrite.
    return;
  }
  [self.juliaVariants setObject:jv forKey:jv.juliaexe];

  // Track the latest known tagged variant.
  if (self.latestKnownTaggedJulia == nil) {
    self.latestKnownTaggedJulia = jv;
  } else {
    JuliaVariant *latestJv = self.latestKnownTaggedJulia;
    NSArray<NSNumber *> *latestV = latestJv.versionComponents;
    NSArray<NSNumber *> *vc = jv.versionComponents;
    if (vc != nil) {
      // Compare version tuple.
      if ([vc[0] isGreaterThan:latestV[0]] ||
          ([vc[0] isEqualToNumber:latestV[0]] &&
           [vc[1] isGreaterThan:latestV[1]]) ||
          ([vc[0] isEqualToNumber:latestV[0]] &&
           [vc[1] isEqualToNumber:latestV[1]] &&
           [vc[2] isGreaterThan:latestV[2]])) {
        latestJv = jv;
        latestV = vc;
      }
    }
    if (latestJv != self.latestKnownTaggedJulia) {
      self.latestKnownTaggedJulia = latestJv;
    }
  }
}

- (void)findJuliaQueryDidUpdate:(NSNotification *)sender {
  if (sender.object != _mdq) {
    return;
  }

  // Disable updates while enumerating results.
  [_mdq disableUpdates];

  for (NSUInteger i = 0; i < _mdq.resultCount; ++i) {
    NSMetadataItem *item = [_mdq resultAtIndex:i];
    // Grab the path attribute from the item.
    NSString *itemPath = [item valueForAttribute:NSMetadataItemPathKey];
    NSString *contentType =
        [item valueForAttribute:(NSString *)kMDItemContentType];

    if ([contentType isEqual:@"public.unix-executable"]) {
      // TODO: Verify the executable is actually a Julia.
      NSURL *juliaexe =
          [[[NSURL alloc] initFileURLWithPath:itemPath
                                  isDirectory:NO] URLByStandardizingPath];
      NSLog(@"Found Julia %@", juliaexe);
      JuliaVariant *jv = [[JuliaVariant alloc] initWithJulia:juliaexe
                                                      bundle:nil];
      [self addJuliaVariant:jv];
    } else if ([contentType isEqual:@"com.apple.framework"]) {
      NSURL *frameworkPath = [[NSURL alloc] initFileURLWithPath:itemPath
                                                    isDirectory:YES];
      NSLog(@"Found Julia framework %@", frameworkPath);

      // Iterate over versions within the framework.

      NSFileManager *fm = NSFileManager.defaultManager;
      NSURL *frameworkVersions =
          [frameworkPath URLByAppendingPathComponent:@"Versions"
                                         isDirectory:YES];
      NSArray<NSURL *> *versions =
          [fm contentsOfDirectoryAtURL:frameworkVersions
              includingPropertiesForKeys:@[ NSURLIsDirectoryKey ]
                                 options:0
                                   error:nil];

      for (NSURL *frameworkVersion in versions) {
        NSNumber *isDir;
        if (![frameworkVersion getResourceValue:&isDir
                                         forKey:NSURLIsDirectoryKey
                                          error:nil] ||
            !isDir.boolValue) {
          // Version is a symink (probably Current) so skip it.
          continue;
        }

        NSBundle *bundle = [NSBundle bundleWithURL:frameworkVersion];

        // Form the path to julia in the framework's Helpers directory.
        NSURL *juliaexe = [[[[bundle.executableURL URLByStandardizingPath]
            URLByDeletingLastPathComponent]
            URLByAppendingPathComponent:@"Helpers"
                            isDirectory:YES]
            URLByAppendingPathComponent:@"julia"
                            isDirectory:NO];

        if (juliaexe == nil) {
          continue;
        }

        NSLog(@"Found Julia %@ (%@) at %@",
              bundle.infoDictionary[@"CFBundleShortVersionString"],
              bundle.infoDictionary[(NSString *)kCFBundleVersionKey], juliaexe);

        JuliaVariant *jv = [[JuliaVariant alloc] initWithJulia:juliaexe
                                                        bundle:bundle];
        [self addJuliaVariant:jv];
      }
    } else {
      NSLog(@"Ignoring Julia at %@ with content type %@", itemPath,
            contentType);
    }
  }

  if (sender.name == NSMetadataQueryDidFinishGatheringNotification) {
    // Initial search is complete after app launch so exec julia.
    NSURL *juliaexe = self.latestKnownTaggedJulia.juliaexe;
    if (juliaexe) {
      execJuliaInTerminal(juliaexe);
    }
  }

  // Safe to enable updates now.
  [_mdq enableUpdates];
}

/// Start a Spotlight query for Julia frameworks.
- (void)findJuliaWithSpotlight {
  if (_mdq != nil) {
    // Query exists so return.
    return;
  }

  _mdq = [[NSMetadataQuery alloc] init];

  // Search for the framework bundle identifier.
  NSPredicate *searchPredicate = [NSPredicate
      predicateWithFormat:
          @"(kMDItemCFBundleIdentifier == 'org.julialang.julia.lib' && "
          @"kMDItemContentType == 'com.apple.framework') || (kMDItemFSName == "
          @"'julia' && kMDItemContentType == 'public.unix-executable')"];
  _mdq.predicate = searchPredicate;

  // Observe the query's notifications.
  [[NSNotificationCenter defaultCenter]
      addObserver:self
         selector:@selector(findJuliaQueryDidUpdate:)
             name:NSMetadataQueryDidUpdateNotification
           object:_mdq];
  [[NSNotificationCenter defaultCenter]
      addObserver:self
         selector:@selector(findJuliaQueryDidUpdate:)
             name:NSMetadataQueryDidFinishGatheringNotification
           object:_mdq];

  if (![_mdq startQuery]) {
    NSAlert *a = [[NSAlert alloc] init];
    a.alertStyle = NSAlertStyleCritical;
    a.messageText = NSLocalizedString(@"Cannot find the Julia framework.", );
    a.informativeText =
        NSLocalizedString(@"The Julia framework cannot be found. The Spotlight "
                          @"query for the Julia framework failed to start.", );
    dispatch_async(dispatch_get_main_queue(), ^{
      [a runModal];
    });
    NSLog(@"Failed starting Julia spotlight search.");
    [self stopFindJuliaWithSpotlight];
  }
}

- (void)stopFindJuliaWithSpotlight {
  if (_mdq == nil) {
    return;
  }

  [_mdq stopQuery];

  [[NSNotificationCenter defaultCenter]
      removeObserver:self
                name:NSMetadataQueryDidUpdateNotification
              object:_mdq];
  [[NSNotificationCenter defaultCenter]
      removeObserver:self
                name:NSMetadataQueryDidFinishGatheringNotification
              object:_mdq];

  _mdq = nil;
}

@end

void execJuliaInTerminal(NSURL *_Nonnull julia) {
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
  NSString *execJuliaCmd =
      [NSString stringWithFormat:@";exec '%s'", juliaFSRepCstr];
  NSAppleEventDescriptor *shellScript =
      [NSAppleEventDescriptor descriptorWithString:execJuliaCmd];
  [aevShellCmd setParamDescriptor:shellScript forKeyword:keyDirectObject];

  if (@available(macOS 10.14, *)) {
    bool retry = false;

    do {
      s = AEDeterminePermissionToAutomateTarget(
          aedTarget.aeDesc, aevShellCmd.eventClass, aevShellCmd.eventID, TRUE);
      retry = s == procNotFound && !retry && launchTerminalApp();
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

bool launchTerminalApp(void) {
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
        return true;
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
        return true;
      } else {
        NSLog(@"LSOpenFromURLSpec failed for %@\n%@", terminalURL,
              [[NSError errorWithDomain:NSOSStatusErrorDomain
                                   code:s
                               userInfo:nil] localizedDescription]);
      }
    }
  }

  return false;
}
