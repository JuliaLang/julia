// This file is a part of Julia. License is MIT: https://julialang.org/license

#import "AppDelegate.h"

/// Terminal's bundle ID.
NSString static const *const terminalBundleID = @"com.apple.Terminal";

static bool launchTerminalApp(void);
static void execJuliaInTerminal(NSURL *julia);

/// Location of an installed variant of Julia (frameowrk or nix hier).
@interface JuliaVariant : NSObject
@property(readonly, nullable) NSBundle *bundle;
@property(readonly, nonnull) NSURL *juliaexe;
@property(readonly, nonnull) NSString *version;
- (instancetype)initWithJulia:(NSURL *)exe bundle:(NSBundle *)b;
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
    // TODO: shell out and make julia tell us its version.
    _version = @"?";
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

@interface AppDelegate ()
@property NSMetadataQuery *mdq;
@property NSMutableDictionary<NSURL *, JuliaVariant *> *juliaVariants;
@property JuliaVariant *latestKnownTaggedJulia;
@end

@implementation AppDelegate

- (instancetype)init {
  self = [super init];
  if (!self) {
    return nil;
  }
  self.juliaVariants = [[NSMutableDictionary alloc] init];
  return self;
}

- (void)dealloc {
  [self stopFindJuliaWithSpotlight];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
  [self findJuliaWithSpotlight];
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
      if (vc[0] > latestV[0] || (vc[0] == latestV[0] && vc[1] > latestV[1]) ||
          (vc[0] == latestV[0] && vc[1] == latestV[1] && vc[2] > latestV[2])) {
        latestJv = jv;
        latestV = vc;
      }
    }
  }
}

- (void)findJuliaQueryDidUpdate:(NSNotification *)sender {
  if (sender.object != self.mdq) {
    return;
  }

  // Disable updates while enumerating results.
  [self.mdq disableUpdates];

  for (NSUInteger i = 0; i < self.mdq.resultCount; ++i) {
    NSMetadataItem *item = [self.mdq resultAtIndex:i];
    // Grab the path attribute from the item.
    NSString *itemPath = [item valueForAttribute:NSMetadataItemPathKey];
    NSString *contentType =
        [item valueForAttribute:(NSString *)kMDItemContentType];

    if ([contentType isEqual:@"public.unix-executable"]) {
      // TODO: Verify the executable is actually a Julia.
      NSURL *juliaexe =
          [[[NSURL alloc] initFileURLWithPath:itemPath
                                  isDirectory:false] URLByStandardizingPath];
      NSLog(@"Found Julia %@", juliaexe);
      JuliaVariant *jv = [[JuliaVariant alloc] initWithJulia:juliaexe
                                                      bundle:nil];
      [self addJuliaVariant:jv];
    } else if ([contentType isEqual:@"com.apple.framework"]) {
      NSURL *frameworkPath = [[NSURL alloc] initFileURLWithPath:itemPath
                                                    isDirectory:true];
      NSLog(@"Found Julia framework %@", frameworkPath);

      // Iterate over versions within the framework.

      NSFileManager *fm = NSFileManager.defaultManager;
      NSURL *frameworkVersions =
          [frameworkPath URLByAppendingPathComponent:@"Versions"
                                         isDirectory:true];
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
                            isDirectory:true]
            URLByAppendingPathComponent:@"julia"
                            isDirectory:false];

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
  [self.mdq enableUpdates];
}

/// Start a Spotlight query for Julia frameworks.
- (void)findJuliaWithSpotlight {
  if (self.mdq != nil) {
    // Query exists so return.
    return;
  }

  self.mdq = [[NSMetadataQuery alloc] init];

  // Search for the framework bundle identifier.
  NSPredicate *searchPredicate = [NSPredicate
      predicateWithFormat:
          @"(kMDItemCFBundleIdentifier == 'org.julialang.julia.lib' && "
          @"kMDItemContentType == 'com.apple.framework') || (kMDItemFSName == "
          @"'julia' && kMDItemContentType == 'public.unix-executable')"];
  self.mdq.predicate = searchPredicate;

  // Observe the query's notifications.
  [[NSNotificationCenter defaultCenter]
      addObserver:self
         selector:@selector(findJuliaQueryDidUpdate:)
             name:NSMetadataQueryDidUpdateNotification
           object:self.mdq];
  [[NSNotificationCenter defaultCenter]
      addObserver:self
         selector:@selector(findJuliaQueryDidUpdate:)
             name:NSMetadataQueryDidFinishGatheringNotification
           object:self.mdq];

  if (![self.mdq startQuery]) {
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
  if (self.mdq == nil) {
    return;
  }

  [self.mdq stopQuery];

  [[NSNotificationCenter defaultCenter]
      removeObserver:self
                name:NSMetadataQueryDidUpdateNotification
              object:self.mdq];
  [[NSNotificationCenter defaultCenter]
      removeObserver:self
                name:NSMetadataQueryDidFinishGatheringNotification
              object:self.mdq];

  self.mdq = nil;
}

@end

void execJuliaInTerminal(NSURL *julia) {
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
          aedTarget.aeDesc, aevShellCmd.eventClass, aevShellCmd.eventID, true);
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
