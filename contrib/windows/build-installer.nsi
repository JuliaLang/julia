Name "The Julia Language"
OutFile "julia-installer.exe"
SetCompress off
CRCCheck on
SetDataBlockOptimize on
ShowInstDetails show
RequestExecutionLevel user

#!if ${Arch} == "x86_64"
#    !define PROGBASE "$PROGRAMFILES64"
#!else
#    !define PROGBASE "$PROGRAMFILES"
#!endif

InstallDir "$LOCALAPPDATA\Julia ${Version}"
DirText "Julia may be installed in a home directory or portable drive. Administrative privileges are not required."

Section
    SetOutPath $INSTDIR
    File /a /r "julia-${Commit}\*"
    CreateShortcut "$INSTDIR\julia.lnk" "$INSTDIR\bin\julia-readline.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"
    ExecShell "open" "$INSTDIR"
SectionEnd
 
Section "uninstall"
    Delete "$INSTDIR/uninstall.exe"
    RMDir /r "$INSTDIR/"
SectionEnd
