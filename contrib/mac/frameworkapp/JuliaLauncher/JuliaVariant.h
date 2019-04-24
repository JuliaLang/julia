// This file is a part of Julia. License is MIT: https://julialang.org/license

@import Foundation;

/// Location of an installed variant of Julia (frameowrk or nix hier).
@interface JuliaVariant : NSObject
@property(readonly, nullable) NSBundle *bundle;
@property(readonly, nonnull) NSURL *juliaexe;
@property(readonly, nonnull) NSURL *canonicalLocation;
@property BOOL defaultVariant;
@property(readonly, nonnull) NSString *status;
@property(readonly, nonnull) NSImage *statusImage;
@property(readonly, nullable) NSString *version;
@property(readonly) BOOL updatingVersion;
- (instancetype _Nonnull)initWithJulia:(NSURL *_Nonnull)exe
                                bundle:(NSBundle *_Nullable)b;
/// (major,minor,patch) components parsed from version.
@property(readonly, nullable) NSArray<NSNumber *> *versionComponents;
@end

@interface JuliaVariantController : NSObject

@property(readonly, nonnull) NSMutableArray<JuliaVariant *> *juliaVariants;
@property(readonly, nonnull)
    NSMutableArray<JuliaVariant *> *hiddenJuliaVariants;
@property(nonatomic, nullable) JuliaVariant *defaultJuliaVariant;
@property(readonly, nullable) JuliaVariant *latestKnownTaggedJulia;
@property(readonly, nullable) NSWindow *variantsWindow;

- (void)askForDefault;
- (BOOL)addJuliaVariant:(JuliaVariant *_Nonnull)jv
                persist:(BOOL)persist
                 unhide:(BOOL)unhide;
- (void)hideJuliaVariants:(NSArray<JuliaVariant *> *_Nonnull)jvs;
- (void)readFromDefaults;
- (void)findJuliaWithSpotlight;
- (void)stopFindJuliaWithSpotlight;
@end
