// This file is a part of Julia. License is MIT: https://julialang.org/license

@import AppKit;
#import "JuliaVariant.h"

@interface JuliaServiceController : NSObject
@property JuliaVariant *juliaVariant;
- (instancetype)initWithJuliaVariant:(JuliaVariant *)jv;
@end
