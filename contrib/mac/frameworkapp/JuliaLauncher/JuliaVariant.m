// This file is a part of Julia. License is MIT: https://julialang.org/license

@import AppKit;
#import "JuliaVariant.h"
#import "AppDelegate.h"
#import "ExecSandbox.h"
#import "JuliaServiceController.h"
#import "VariantViewController.h"

NSString *const KnownJuliaVariantsKey = @"KnownJuliaVariantsKey";
NSString *const HiddenJuliaVariantsKey = @"HiddenJuliaVariantsKey";

@implementation JuliaVariant

- (BOOL)isEqual:(id)object {
  if ([object isKindOfClass:[JuliaVariant class]]) {
    JuliaVariant *jv = object;
    return [self.juliaexe isEqual:jv.juliaexe];
  }
  return NO;
}

- (NSUInteger)hash {
  return [self.juliaexe hash];
}

- (NSURL *)canonicalLocation {
  return _bundle ? _bundle.bundleURL : _juliaexe;
}

- (instancetype)initWithJulia:(NSURL *)exe bundle:(NSBundle *)b {
  self = [super init];
  if (!self) {
    return nil;
  }
  NSNumber *isExecutable = nil;
  if (![exe getResourceValue:&isExecutable
                      forKey:NSURLIsExecutableKey
                       error:nil] ||
      ![isExecutable boolValue]) {
    return nil;
  }
  _juliaexe = exe;
  _bundle = b;
  _defaultVariant = NO;
  _version = nil;
  _status = @"";
  _statusImage = [NSImage imageNamed:NSImageNameStatusNone];
  if (_bundle == nil) {
    // Try to locate the framework bundle.
    NSURL *frameworkURL =
        [[exe URLByDeletingLastPathComponent] URLByDeletingLastPathComponent];
    NSBundle *bundle = [NSBundle bundleWithURL:frameworkURL];
    if (bundle && [[bundle bundleIdentifier]
                      isEqualToString:@"org.julialang.julia.lib"]) {
      _bundle = bundle;
    }
  }
  if (_bundle) {
    // Extract version from framework bundle.
    _version = _bundle.infoDictionary[(NSString *)kCFBundleVersionKey];
  }

  // Exec the julia and have it tell us its version.

  NSData *juliaexeBookmark = [_juliaexe bookmarkDataWithOptions:0
                                 includingResourceValuesForKeys:nil
                                                  relativeToURL:nil
                                                          error:nil];

  _updatingVersion = YES;

  id<ExecSandboxProtocol> remote = [[ExecSandboxController sharedController]
      remoteObjectProxyWithErrorHandler:^(NSError *error) {
        dispatch_async(dispatch_get_main_queue(), ^{
          [self willChangeValueForKey:@"updatingVersion"];
          self->_updatingVersion = NO;
          [self didChangeValueForKey:@"updatingVersion"];
        });
        NSLog(@"ExecSandbox remote failed: %@", error);
      }];

  [remote eval:@"print(\"$(Base.VERSION.major).$(Base.VERSION.minor).$(Base."
               @"VERSION.patch)\")"
      withJulia:juliaexeBookmark
      arguments:nil
           task:^(id<TaskProtocol> task, NSFileHandle *stdIn,
                  NSFileHandle *stdOut, NSFileHandle *stdErr) {
             [task launch:^(int status, BOOL crashed) {
               dispatch_async(dispatch_get_main_queue(), ^{
                 NSString *vout =
                     [[NSString alloc] initWithData:[stdOut readDataToEndOfFile]
                                           encoding:NSUTF8StringEncoding];
                 NSString *verr =
                     [[NSString alloc] initWithData:[stdErr readDataToEndOfFile]
                                           encoding:NSUTF8StringEncoding];
                 if (status == 0) {
                   if (vout && [vout length] > 0) {
                     if ([vout isEqualToString:self->_version]) {
                       [self willChangeValueForKey:@"statusImage"];
                       [self willChangeValueForKey:@"status"];
                       [self willChangeValueForKey:@"updatingVersion"];
                       self->_statusImage =
                           [NSImage imageNamed:NSImageNameStatusAvailable];
                       self->_status = @"Consistent";
                       self->_updatingVersion = NO;
                       [self didChangeValueForKey:@"updatingVersion"];
                       [self didChangeValueForKey:@"status"];
                       [self didChangeValueForKey:@"statusImage"];
                     } else {
                       [self willChangeValueForKey:@"statusImage"];
                       [self willChangeValueForKey:@"status"];
                       [self willChangeValueForKey:@"version"];
                       [self willChangeValueForKey:@"updatingVersion"];
                       self->_statusImage = [NSImage
                           imageNamed:NSImageNameStatusPartiallyAvailable];
                       self->_status = @"Inconsistent version";
                       self->_version = vout;
                       self->_updatingVersion = NO;
                       [self didChangeValueForKey:@"updatingVersion"];
                       [self didChangeValueForKey:@"version"];
                       [self didChangeValueForKey:@"status"];
                       [self didChangeValueForKey:@"statusImage"];
                     }
                   } else {
                     [self willChangeValueForKey:@"statusImage"];
                     [self willChangeValueForKey:@"status"];
                     [self willChangeValueForKey:@"updatingVersion"];
                     self->_statusImage =
                         [NSImage imageNamed:NSImageNameCaution];
                     self->_updatingVersion = NO;
                     self->_status = @"Corrupt—no version reported";
                     [self didChangeValueForKey:@"updatingVersion"];
                     [self didChangeValueForKey:@"status"];
                     [self didChangeValueForKey:@"statusImage"];
                     NSLog(@"Successful exit but no output. Maybe not julia?");
                   }
                 } else {
                   NSLog(@"Failed getting version of %@\n%@", exe, verr);
                   [self willChangeValueForKey:@"statusImage"];
                   [self willChangeValueForKey:@"status"];
                   [self willChangeValueForKey:@"updatingVersion"];
                   self->_statusImage =
                       [NSImage imageNamed:NSImageNameStatusUnavailable];
                   self->_updatingVersion = NO;
                   if (crashed) {
                     self->_status = @"Corrupt—crashed";
                   } else {
                     self->_status = @"Corrupt—abnormal exit";
                   }
                   [self didChangeValueForKey:@"updatingVersion"];
                   [self didChangeValueForKey:@"status"];
                   [self didChangeValueForKey:@"statusImage"];
                 }
               });
             }];
           }];

  NSLog(@"Getting version by execing %@", exe);

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

+ (NSSet<NSString *> *)keyPathsForValuesAffectingValueForKey:(NSString *)key {
  NSSet *keyPaths = [super keyPathsForValuesAffectingValueForKey:key];
  if ([key isEqualToString:@"versionComponents"]) {
    keyPaths = [keyPaths setByAddingObjectsFromArray:@[ @"version" ]];
  }
  return keyPaths;
}

@end

@interface JuliaVariantController () {
  NSMetadataQuery *_Nullable _mdq;
  NSWindow *_Nullable _variantsWindow;
  VariantViewController *_Nullable _variantsVC;
}
@property(nonnull) NSMutableArray<JuliaVariant *> *juliaVariants;
@property(nullable) JuliaVariant *latestKnownTaggedJulia;
- (void)writeToDefaults;
- (void)findJuliaQueryDidUpdate:(NSNotification *)sender;
@end

@implementation JuliaVariantController

- (instancetype)init {
  self = [super init];
  if (!self) {
    return nil;
  }
  _juliaVariants = [[NSMutableArray alloc] init];
  _hiddenJuliaVariants = [[NSMutableArray alloc] init];
  return self;
}

- (void)dealloc {
  [self stopFindJuliaWithSpotlight];
}

- (NSWindow *)variantsWindow {
  if (!_variantsWindow) {
    _variantsVC = [[VariantViewController alloc] initWithController:self];
    _variantsWindow = [VariantViewController makeWindow:_variantsVC];
    _variantsWindow.releasedWhenClosed = false;
  }
  return _variantsWindow;
}

- (void)setDefaultJuliaVariant:(JuliaVariant *)jv {
  NSAssert(!jv || [self.juliaVariants containsObject:jv],
           @"Default julia variant must be added first.");
  if (_defaultJuliaVariant == jv) {
    return;
  }
  [self willChangeValueForKey:@"defaultJuliaVariant"];
  _defaultJuliaVariant.defaultVariant = NO;
  jv.defaultVariant = YES;
  _defaultJuliaVariant = jv;
  [self writeToDefaults];
  NSApp.servicesProvider =
      [[JuliaServiceController alloc] initWithJuliaVariant:jv];
  [self didChangeValueForKey:@"defaultJuliaVariant"];
}

- (BOOL)addJuliaVariant:(JuliaVariant *_Nonnull)jv
                persist:(BOOL)persist
                 unhide:(BOOL)unhide {
  if ([self.juliaVariants containsObject:jv]) {
    // Don't overwrite.
    return NO;
  }
  if ([self.hiddenJuliaVariants containsObject:jv]) {
    if (!unhide) {
      return NO;
    } else {
      [self.hiddenJuliaVariants removeObject:jv];
    }
  }
  [self.juliaVariants addObject:jv];

  // Track the latest known tagged variant.
  if (self.latestKnownTaggedJulia == nil) {
    self.latestKnownTaggedJulia = jv;
  } else {
    JuliaVariant *latestJv = self.latestKnownTaggedJulia;
    NSArray<NSNumber *> *latestV = latestJv.versionComponents;
    NSArray<NSNumber *> *vc = jv.versionComponents;
    if (vc != nil) {
      // Compare version tuple.
      if (latestV == nil || ([vc[0] isGreaterThan:latestV[0]] ||
                             ([vc[0] isEqualToNumber:latestV[0]] &&
                              [vc[1] isGreaterThan:latestV[1]]) ||
                             ([vc[0] isEqualToNumber:latestV[0]] &&
                              [vc[1] isEqualToNumber:latestV[1]] &&
                              [vc[2] isGreaterThan:latestV[2]]))) {
        latestJv = jv;
        latestV = vc;
      }
    }
    if (latestJv != self.latestKnownTaggedJulia) {
      self.latestKnownTaggedJulia = latestJv;
    }
  }

  if (persist) {
    [self writeToDefaults];
  }

  [_variantsVC reload];

  return YES;
}

- (void)hideJuliaVariants:(NSArray<JuliaVariant *> *_Nonnull)jvs {
  BOOL updated = NO;

  for (JuliaVariant *hidejv in jvs) {
    if ([self.hiddenJuliaVariants containsObject:hidejv]) {
      continue;
    }
    if (self.defaultJuliaVariant == hidejv) {
      self.defaultJuliaVariant = nil;
    }
    if (self.latestKnownTaggedJulia == hidejv) {
      self.latestKnownTaggedJulia = nil;
    }
    [self.juliaVariants removeObject:hidejv];
    [self.hiddenJuliaVariants addObject:hidejv];
    updated = YES;
  }

  if (!updated) {
    return;
  }

  // Track the latest known tagged variant.
  if (self.latestKnownTaggedJulia == nil && self.juliaVariants.count > 0) {
    for (JuliaVariant *jv in self.juliaVariants) {
      if (self.latestKnownTaggedJulia == nil) {
        self.latestKnownTaggedJulia = jv;
      } else {
        JuliaVariant *latestJv = self.latestKnownTaggedJulia;
        NSArray<NSNumber *> *latestV = latestJv.versionComponents;
        NSArray<NSNumber *> *vc = jv.versionComponents;
        if (vc != nil) {
          // Compare version tuple.
          if (latestV == nil || ([vc[0] isGreaterThan:latestV[0]] ||
                                 ([vc[0] isEqualToNumber:latestV[0]] &&
                                  [vc[1] isGreaterThan:latestV[1]]) ||
                                 ([vc[0] isEqualToNumber:latestV[0]] &&
                                  [vc[1] isEqualToNumber:latestV[1]] &&
                                  [vc[2] isGreaterThan:latestV[2]]))) {
            latestJv = jv;
            latestV = vc;
          }
        }
        if (latestJv != self.latestKnownTaggedJulia) {
          self.latestKnownTaggedJulia = latestJv;
        }
      }
    }
  }

  [self writeToDefaults];

  [_variantsVC reload];

  if (self.defaultJuliaVariant == nil) {
    [self.variantsWindow makeKeyAndOrderFront:nil];
    [_variantsVC askForDefault];
  }
}

- (void)readFromDefaults {
  JuliaVariant *newDefaultJV = nil;

  {
    NSArray<NSData *> *knownJvs = [[NSUserDefaults standardUserDefaults]
        arrayForKey:KnownJuliaVariantsKey];

    for (NSData *jvBookmark in knownJvs) {
      if (![jvBookmark isKindOfClass:[NSData class]]) {
        continue;
      }
      NSURL *exeURL = [NSURL URLByResolvingBookmarkData:jvBookmark
                                                options:0
                                          relativeToURL:nil
                                    bookmarkDataIsStale:nil
                                                  error:nil];
      if (!exeURL) {
        NSLog(@"Failed resolving URL bookmark for julia variant in Defaults.");
        continue;
      }
      JuliaVariant *jv = [[JuliaVariant alloc] initWithJulia:exeURL bundle:nil];
      if (jv) {
        [self addJuliaVariant:jv persist:NO unhide:YES];
        // By convention, the first bookmark corresponds to the default Julia.
        if (jvBookmark == knownJvs[0]) {
          // Don't set self.defaultJuliaVariant yet since that will write to
          // defaults.
          newDefaultJV = jv;
        }
      }
    }
  }

  {
    NSArray<NSData *> *hiddenJvs = [[NSUserDefaults standardUserDefaults]
        arrayForKey:HiddenJuliaVariantsKey];

    for (NSData *jvBookmark in hiddenJvs) {
      if (![jvBookmark isKindOfClass:[NSData class]]) {
        continue;
      }
      NSURL *exeURL = [NSURL URLByResolvingBookmarkData:jvBookmark
                                                options:0
                                          relativeToURL:nil
                                    bookmarkDataIsStale:nil
                                                  error:nil];
      if (!exeURL) {
        NSLog(@"Failed resolving URL bookmark for julia variant in Defaults.");
        continue;
      }
      JuliaVariant *jv = [[JuliaVariant alloc] initWithJulia:exeURL bundle:nil];
      if (jv) {
        [self.hiddenJuliaVariants addObject:jv];
      }
    }
  }

  // NB: setting self.defaultJuliaVariant will writeToDefaults here.
  self.defaultJuliaVariant = newDefaultJV;
}

- (void)writeToDefaults {
  {
    JuliaVariant *defaultJv = self.defaultJuliaVariant;
    NSData *defaultJVBookmark =
        [defaultJv.juliaexe bookmarkDataWithOptions:0
                     includingResourceValuesForKeys:nil
                                      relativeToURL:nil
                                              error:nil];
    if (!defaultJVBookmark) {
      defaultJv = nil;
    }

    NSMutableArray<NSData *> *knownJvs = [[NSMutableArray alloc] init];
    if (defaultJVBookmark) {
      [knownJvs addObject:defaultJVBookmark];
    }

    for (JuliaVariant *jv in self.juliaVariants) {
      if (jv == defaultJv) {
        // Already first item in array so skip it.
        continue;
      }
      NSData *jvBookmark = [jv.juliaexe bookmarkDataWithOptions:0
                                 includingResourceValuesForKeys:nil
                                                  relativeToURL:nil
                                                          error:nil];
      if (jvBookmark) {
        [knownJvs addObject:jvBookmark];
      }
    }

    [[NSUserDefaults standardUserDefaults] setObject:knownJvs
                                              forKey:KnownJuliaVariantsKey];
  }

  {
    NSMutableArray<NSData *> *jvsBookmarks = [[NSMutableArray alloc] init];

    for (JuliaVariant *jv in self.hiddenJuliaVariants) {
      NSData *jvBookmark = [jv.juliaexe bookmarkDataWithOptions:0
                                 includingResourceValuesForKeys:nil
                                                  relativeToURL:nil
                                                          error:nil];
      if (jvBookmark) {
        [jvsBookmarks addObject:jvBookmark];
      }
    }

    [[NSUserDefaults standardUserDefaults] setObject:jvsBookmarks
                                              forKey:HiddenJuliaVariantsKey];
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

    if ([contentType isEqualToString:@"public.unix-executable"]) {
      NSURL *juliaexe =
          [[[NSURL alloc] initFileURLWithPath:itemPath
                                  isDirectory:NO] URLByStandardizingPath];
      NSLog(@"Found Julia %@", juliaexe);
      JuliaVariant *jv = [[JuliaVariant alloc] initWithJulia:juliaexe
                                                      bundle:nil];
      if (jv) {
        [self addJuliaVariant:jv persist:NO unhide:NO];
      }
    } else if ([contentType isEqualToString:@"com.apple.framework"]) {
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
        if (jv) {
          [self addJuliaVariant:jv persist:NO unhide:NO];
        }
      }
    } else {
      NSLog(@"Ignoring Julia at %@ with content type %@", itemPath,
            contentType);
    }
  }

  [self writeToDefaults];

  if (!self.defaultJuliaVariant &&
      [sender.name
          isEqualToString:NSMetadataQueryDidFinishGatheringNotification]) {
    // This is the initial query update and no default exists yet.

    // Try to set a reasonable default julia.
    self.defaultJuliaVariant =
        self.latestKnownTaggedJulia
            ? self.latestKnownTaggedJulia
            : (self.juliaVariants.count > 0 ? self.juliaVariants[0] : nil);

    [self.variantsWindow makeKeyAndOrderFront:nil];
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
