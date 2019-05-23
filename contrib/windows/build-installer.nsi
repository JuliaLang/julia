Unicode true

!include "MUI2.nsh"
!include "nsDialogs.nsh"
!include "winmessages.nsh"

OutFile "julia-installer.exe"
SetCompress off
CRCCheck off
SetDataBlockOptimize on
ShowInstDetails nevershow
ShowUninstDetails nevershow
RequestExecutionLevel user
BrandingText " "

!define /date YEAR "%Y"

Name "Julia"
VIProductVersion "10.20.0.0" # arbitrary value since it doesn't mater, but is required; format must be X.X.X.X
VIAddVersionKey "ProductName" "Julia"
VIAddVersionKey "CompanyName " "Julia Language"
VIAddVersionKey "ProductVersion" "${Version}"
VIAddVersionKey "FileDescription" "Julia Language Installer"
VIAddVersionKey "Comments" "https://julialang.org/"
VIAddVersionKey "LegalCopyright" "Copyright (c) 2009-${YEAR} Julia Language"
VIAddVersionKey "FileVersion" ""

Caption "Julia Installer" # title bar

!define MUI_ICON "${JULIAHOME}\contrib\windows\julia.ico"
!define MUI_UNICON "${JULIAHOME}\contrib\windows\julia.ico"
!define MUI_WELCOMEFINISHPAGE_BITMAP "${JULIAHOME}\contrib\windows\julia-banner.bmp"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${JULIAHOME}\contrib\windows\julia-header.bmp"
!define MUI_HEADERIMAGE_RIGHT

# Uninstall settings
!define UninstLog "uninstall.log"
var UninstLog

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
    CreateShortCut "$DESKTOP\julia.lnk" "$INSTDIR\bin\julia.exe" "" "$INSTDIR\bin\julia.exe" 0
  ${EndIf}
FunctionEnd

# Variable definitions used in installer pages
InstallDir "$LOCALAPPDATA\Julia-${Version}"
!define JuliaStartMenuFolder "Julia ${Version}"

# Page settings
# Note that we repurpose the checkboxes on the FinishPage in order to keep it simple.
!define MUI_DIRECTORYPAGE_TEXT_TOP "Julia may be installed in any accessible directory.$\r$\n$\r$\nPlease run installer as Administrator to install Julia system-wide."
!define MUI_FINISHPAGE_SHOWREADME
!define MUI_FINISHPAGE_SHOWREADME_TEXT "Create Start Menu folder and shortcut"
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION AddToStartMenu

!define MUI_WELCOMEPAGE_TITLE "Welcome to Julia ${Version}"
!define MUI_WELCOMEPAGE_TEXT  "Setup will guide you through installation.$\r$\n$\r$\nClick Next to continue."
!define MUI_FINISHPAGE_TITLE "Julia installation complete"
!define MUI_FINISHPAGE_TEXT "Julia has been successfully installed.$\r$\n$\r$\nClick Finish to close the installer."

!define MUI_FINISHPAGE_RUN
!define MUI_FINISHPAGE_RUN_TEXT "Open the Julia install folder"
!define MUI_FINISHPAGE_RUN_FUNCTION ShowInstallFolder

!define MUI_UNCONFIRMPAGE_TEXT_TOP "Julia will be uninstalled from the following folder."
!define MUI_UNCONFIRMPAGE_TEXT_LOCATION "Uninstalling from"

# Pages to show
!define MUI_PAGE_HEADER_TEXT "Choose Installation Directory"
!define MUI_PAGE_HEADER_SUBTEXT ""
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
Section
!insertmacro MUI_HEADER_TEXT "Installing" ""
SectionEnd

!define MUI_PAGE_CUSTOMFUNCTION_SHOW desktopCheckbox
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE createDesktopLink
!insertmacro MUI_PAGE_FINISH

!define MUI_PAGE_HEADER_TEXT "Uninstall Julia"
!define MUI_PAGE_HEADER_SUBTEXT ""
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"

# Add/Remove Programs entry
!define ARP "Software\Microsoft\Windows\CurrentVersion\Uninstall\Julia ${Version}"

Section "Dummy Section" SecDummy
    SetOutPath $INSTDIR
    File /a /r "julia-${Commit}\*"
    WriteUninstaller "$INSTDIR\uninstall.exe"
    CreateShortcut "$INSTDIR\julia.lnk" "$INSTDIR\bin\julia.exe" "" "$INSTDIR\bin\julia.exe" 0

    # ARP entries
    WriteRegStr HKCU "${ARP}" \
                 "DisplayName" "Julia ${Version}"
    WriteRegStr HKCU "${ARP}" \
                 "Publisher" "Julia Language"
    WriteRegStr HKCU "${ARP}" \
                 "DisplayIcon" "$INSTDIR\bin\julia.exe"
    WriteRegStr HKCU "${ARP}" \
                 "UninstallString" "$\"$INSTDIR\uninstall.exe$\""
    WriteRegStr HKCU "${ARP}" \
                 "QuietUninstallString" "$\"$INSTDIR\uninstall.exe$\" /S"
    WriteRegDWORD HKCU "${ARP}" "EstimatedSize" "300"
    WriteRegDWORD HKCU "${ARP}" "NoModify" "1"
    WriteRegDWORD HKCU "${ARP}" "NoRepair" "1"
SectionEnd

Section "uninstall"
    Delete "$DESKTOP\julia.lnk"
    Delete "$INSTDIR\julia.lnk"
    DeleteRegKey HKCU "${ARP}"

    # Remove Start Menu entries
    Delete "$SMPROGRAMS\${JuliaStartMenuFolder}\julia.lnk"
    Delete "$SMPROGRAMS\${JuliaStartMenuFolder}\Uninstall.lnk"
    RMDir "$SMPROGRAMS\${JuliaStartMenuFolder}"


    # Remove only files listed in uninstall log
    IfFileExists "$INSTDIR\etc\${UninstLog}" +3
        MessageBox MB_OK|MB_ICONSTOP "Missing uninstall log: ${UninstLog}"
            Abort

    Push $R0
    Push $R1
    Push $R2
    SetFileAttributes "$INSTDIR\etc\${UninstLog}" NORMAL
    FileOpen $UninstLog "$INSTDIR\etc\${UninstLog}" r
    StrCpy $R1 -1

    GetLineCount:
        ClearErrors
        FileRead $UninstLog $R0
        IntOp $R1 $R1 + 1
        StrCpy $R0 $R0 -2
        Push $R0
        IfErrors 0 GetLineCount
    Pop $R0

    LoopRead:
        StrCmp $R1 0 LoopDone
        Pop $R0

        IfFileExists "$INSTDIR\$R0\*.*" 0 +3
            RMDir "$INSTDIR\$R0"  #is dir
        Goto +3
        IfFileExists "$INSTDIR\$R0" 0 +2
            Delete "$INSTDIR\$R0" #is file

        IntOp $R1 $R1 - 1
        Goto LoopRead
    LoopDone:
    FileClose $UninstLog
    Delete "$INSTDIR\etc\${UninstLog}"
    RMDir "$INSTDIR\etc"
    Delete "$INSTDIR\uninstall.exe"
    SetOutPath $DESKTOP
    RMDir "$INSTDIR"

    Pop $R2
    Pop $R1
    Pop $R0
    # End of file deletion section
SectionEnd

# Helper function to create Start Menu folder and shortcuts
Function AddToStartMenu
    CreateDirectory "$SMPROGRAMS\${JuliaStartMenuFolder}"
    CreateShortcut "$SMPROGRAMS\${JuliaStartMenuFolder}\julia-${Version}.lnk" "$INSTDIR\julia.lnk" "" "" "" "" "" "Julia"
    CreateShortcut "$SMPROGRAMS\${JuliaStartMenuFolder}\Uninstall-Julia-${Version}.lnk" "$instdir\uninstall.exe"
FunctionEnd

# Opens the installation folder
Function ShowInstallFolder
    ExecShell "open" $INSTDIR
FunctionEnd
