!include "MUI2.nsh"
!include "nsDialogs.nsh"
!include "winmessages.nsh"

Name "The Julia Language"
OutFile "julia-installer.exe"
SetCompress off
CRCCheck on
SetDataBlockOptimize on
ShowInstDetails show
RequestExecutionLevel user
BrandingText "Julia ${Version}"

# User interface changes
var Checkbox

# Add the desktop checkbox to the final page.
Function desktopCheckbox
  ${NSD_CreateCheckbox} 120u 130u 100% 10u "Create &desktop shortcut"
  Pop $Checkbox
  SetCtlColors $Checkbox "" "ffffff"
FunctionEnd

# Create the desktop link only, if the desktop checkbox is active.
Function createDesktopLink
  ${NSD_GetState} $Checkbox $0
  ${If} $0 <> 0
    CreateShortCut "$DESKTOP\Julia.lnk" "$INSTDIR\bin\julia.exe"
  ${EndIf}
FunctionEnd

# Icon settings
!define MUI_ICON "contrib\windows\julia.ico"

# Variable definitions used in installer pages
InstallDir "$LOCALAPPDATA\Julia-${Version}"
!define StartMenuFolder "Julia ${Version}" 

# Page settings
# Note that we repurpose the checkboxes on the FinishPage
# in order to keep it simple.
!define MUI_DIRECTORYPAGE_TEXT_TOP "Julia may be installed in any accessible directory, including a home folder or portable device. Please run as Administrator to install for system-wide use."
!define MUI_FINISHPAGE_SHOWREADME
!define MUI_FINISHPAGE_SHOWREADME_TEXT "Create Start Menu folder and shortcut"
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION AddToStartMenu
!define MUI_FINISHPAGE_RUN
!define MUI_FINISHPAGE_RUN_TEXT "Open Julia install folder"
!define MUI_FINISHPAGE_RUN_FUNCTION ShowInstallFolder

# Pages to show

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!define MUI_PAGE_CUSTOMFUNCTION_SHOW desktopCheckbox
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE createDesktopLink
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

Section "Dummy Section" SecDummy
    SetOutPath $INSTDIR
    File /a /r "julia-${Commit}\*"
    WriteUninstaller "$INSTDIR\Uninstall.exe"
    CreateShortcut "$INSTDIR\julia.lnk" "$INSTDIR\bin\julia.exe"
SectionEnd
 
Section "uninstall"
    Delete "$INSTDIR/uninstall.exe"
    Delete "$DESKTOP\Julia.lnk"
    RMDir /r "$SMPROGRAMS\${StartMenuFolder}"
    RMDir /r "$INSTDIR/"
SectionEnd

# Helper function to create Start Menu folder and shortcuts
Function AddToStartMenu
    CreateDirectory "$SMPROGRAMS\${StartMenuFolder}"
    CreateShortcut "$SMPROGRAMS\${StartMenuFolder}\julia.lnk" "$INSTDIR\julia.lnk" "" "" "" "" "" "The Julia Language"
    CreateShortcut "$SMPROGRAMS\${StartMenuFolder}\Uninstall.lnk" "$instdir\Uninstall.exe"
FunctionEnd 

# Opens the installation folder
Function ShowInstallFolder
    ExecShell "open" $INSTDIR
FunctionEnd


