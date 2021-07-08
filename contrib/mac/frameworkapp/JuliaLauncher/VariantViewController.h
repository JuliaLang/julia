// This file is a part of Julia. License is MIT: https://julialang.org/license

@import AppKit;
#import "JuliaVariant.h"

@interface VariantViewController
    : NSViewController <NSTableViewDelegate, NSTableViewDataSource,
                        NSPathControlDelegate>
- (instancetype _Nullable)initWithController:
    (JuliaVariantController *_Nonnull)jvc;
+ (NSWindow *_Nullable)makeWindow:(VariantViewController *_Nonnull)vvc;
@property(weak, nullable, readonly) JuliaVariantController *jvController;
- (void)reload;
- (void)askForDefault;
@end
