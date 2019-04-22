// This file is a part of Julia. License is MIT: https://julialang.org/license

#import "VariantViewController.h"
#import "AppDelegate.h"

static void *const KVOContext = (void *)&KVOContext;

@interface VariantViewController ()
@property(weak) IBOutlet NSTableView *tableView;
@property(weak) IBOutlet NSButton *removeButton;
@property(weak) IBOutlet NSButton *revealButton;
@property(weak) IBOutlet NSButton *runButton;
- (void)updateControlState;
@end

@implementation VariantViewController

- (instancetype)initWithController:(JuliaVariantController *)jvc {
  self = [super initWithNibName:nil bundle:nil];
  if (!self) {
    return nil;
  }
  _jvController = jvc;
  return self;
}

- (NSString *)tableView:(NSTableView *)tableView
    typeSelectStringForTableColumn:(NSTableColumn *)tableColumn
                               row:(NSInteger)row {
  return self.jvController.juliaVariants[row].version;
}

- (BOOL)pathControl:(NSPathControl *)pathControl
     shouldDragItem:(NSPathControlItem *)pathItem
     withPasteboard:(NSPasteboard *)pasteboard API_AVAILABLE(macos(10.10)) {
  return YES;
}

+ (NSWindow *)makeWindow:(VariantViewController *_Nonnull)vvc {
  NSWindow *w = nil;
  if (@available(macOS 10.10, *)) {
    w = [NSWindow windowWithContentViewController:vvc];
  } else {
    w = [[NSWindow alloc] initWithContentRect:NSZeroRect
                                    styleMask:(NSWindowStyleMaskTitled |
                                               NSWindowStyleMaskClosable |
                                               NSWindowStyleMaskMiniaturizable |
                                               NSWindowStyleMaskResizable)
                                      backing:NSBackingStoreBuffered
                                        defer:NO];
    [w bind:@"title" toObject:vvc withKeyPath:@"title" options:nil];
    w.contentView = vvc.view;
    w.contentSize = vvc.view.fittingSize;
    w.releasedWhenClosed = NO;
    [w center];
  }
  if (@available(macOS 10.12, *)) {
    w.tabbingMode = NSWindowTabbingModeDisallowed;
  }
  w.restorable = NO;
  return w;
}

- (void)reload {
  [self.tableView reloadData];
  [self updateControlState];
}

- (void)askForDefault {
  NSAlert *a = [[NSAlert alloc] init];
  a.messageText = NSLocalizedString(@"Choose a default Julia.", );
  a.alertStyle = NSAlertStyleInformational;
  [a beginSheetModalForWindow:self.view.window
                modalDelegate:nil
               didEndSelector:nil
                  contextInfo:nil];
}

- (NSString *)title {
  return NSLocalizedString(@"Versions", );
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView {
  return self.jvController.juliaVariants.count;
}

- (id)tableView:(NSTableView *)tableView
    objectValueForTableColumn:(NSTableColumn *)tableColumn
                          row:(NSInteger)row {
  return self.jvController.juliaVariants[row];
}

- (NSView *)tableView:(NSTableView *)tableView
    viewForTableColumn:(NSTableColumn *)tableColumn
                   row:(NSInteger)row {
  NSTableCellView *tcv = [self.tableView
      makeViewWithIdentifier:[tableColumn.identifier
                                 stringByAppendingString:@"Cell"]
                       owner:self];
  NSAssert(tcv, @"Table cell view must be non-nill.");
  return tcv;
}

- (void)tableViewSelectionDidChange:(NSNotification *)notification {
  [self updateControlState];
}

- (void)updateControlState {
  if (self.tableView.numberOfSelectedRows == 0) {
    self.removeButton.enabled = NO;
    self.revealButton.enabled = NO;
    self.runButton.enabled = NO;
  } else {
    self.removeButton.enabled = YES;
    self.revealButton.enabled = YES;
    self.runButton.enabled = YES;
  }
}

- (IBAction)setDefault:(id)sender {
  self.jvController.defaultJuliaVariant =
      self.jvController.juliaVariants[[self.tableView rowForView:sender]];
}

- (IBAction)run:(id)sender {
  if ([sender isKindOfClass:[NSButton class]]) {
    [self.tableView.selectedRowIndexes
        enumerateIndexesUsingBlock:^(NSUInteger i, BOOL *_Nonnull stop) {
          [AppDelegate
              execJuliaInTerminal:self.jvController.juliaVariants[i].juliaexe
                             exec:YES
                      interactive:YES
                    fileOrProgram:nil];
        }];
  } else if ([sender isKindOfClass:[NSTableView class]]) {
    NSInteger i = self.tableView.clickedRow;
    if (i < 0) {
      return;
    }
    [AppDelegate execJuliaInTerminal:self.jvController.juliaVariants[i].juliaexe
                                exec:YES
                         interactive:YES
                       fileOrProgram:nil];
  }
}

- (IBAction)selectItem:(id)sender {
  NSEvent *e = [NSApp currentEvent];
  BOOL extend = (e.modifierFlags &
                 (NSEventModifierFlagCommand | NSEventModifierFlagShift)) != 0;
  [self.tableView
          selectRowIndexes:[NSIndexSet indexSetWithIndex:[self.tableView
                                                             rowForView:sender]]
      byExtendingSelection:extend];
}

- (IBAction)add:(id)sender {
  NSOpenPanel *p = [NSOpenPanel openPanel];
  p.message = NSLocalizedString(@"Choose a julia executable to add.", );
  p.prompt = NSLocalizedString(@"Add", );
  p.canChooseFiles = YES;
  p.canChooseDirectories = NO;
  p.allowsMultipleSelection = NO;
  p.allowedFileTypes = @[ @"public.unix-executable" ];
  [p beginSheetModalForWindow:self.view.window
            completionHandler:^(NSModalResponse result) {
              if (result != NSFileHandlingPanelOKButton) {
                return;
              }
              JuliaVariant *jv = [[JuliaVariant alloc] initWithJulia:p.URLs[0]
                                                              bundle:nil];
              if (jv && [self.jvController addJuliaVariant:jv
                                                   persist:YES
                                                    unhide:YES]) {
                if (!self.jvController.defaultJuliaVariant) {
                  self.jvController.defaultJuliaVariant = jv;
                }
              }
            }];
}

- (IBAction)remove:(id)sender {
  NSMutableArray<JuliaVariant *> *jvs = [[NSMutableArray alloc] init];
  [self.tableView.selectedRowIndexes
      enumerateIndexesUsingBlock:^(NSUInteger i, BOOL *_Nonnull stop) {
        [jvs addObject:self.jvController.juliaVariants[i]];
      }];
  [self.jvController hideJuliaVariants:jvs];
}

- (IBAction)reveal:(id)sender {
  if ([sender isKindOfClass:[NSPathControl class]]) {
    NSPathControl *pathControl = sender;
    [[NSWorkspace sharedWorkspace]
        activateFileViewerSelectingURLs:@[ pathControl.URL ]];
    return;
  }

  NSMutableArray<NSURL *> *itemUrls = [[NSMutableArray alloc] init];
  [self.tableView.selectedRowIndexes enumerateIndexesUsingBlock:^(
                                         NSUInteger i, BOOL *_Nonnull stop) {
    [itemUrls addObject:self.jvController.juliaVariants[i].canonicalLocation];
  }];
  [[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:itemUrls];
}

@end
