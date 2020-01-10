; preprocessor will fill in the commented out constants below
; #define AppVersion "x.y.z"; Julia version
; #define AppSourceFiles "C:\ex" ; Julia build root directory
; #define AppHomeFiles "C:\ex" ; Julia home directory
#define AppName "Julia"
#define AppMainExe "bin\julia.exe"
#define CurrentYear GetDateTimeString('yyyy', '', '')


[LangOptions]
DialogFontName=Segoe UI
WelcomeFontName=Segoe UI
CopyrightFontName=Segoe UI
TitleFontName=Segoe UI


[Messages]
SetupAppTitle={#AppName} Installer
SetupWindowTitle=Installer - {#AppName} {#AppVersion}
UninstallAppTitle={#AppName} Uninstaller
UninstallAppFullTitle=Uninstaller - {#AppName} {#AppVersion}
WizardSelectDir=Select Installation Directory
SelectDirDesc=
SelectDirLabel3=
SelectDirBrowseLabel=Please run installer as Administrator to install {#AppName} system wide.%n%nTo install into a different folder click Browse.
WizardPreparing=Installing
PreparingDesc=
InstallingLabel=
ClickFinish=
FinishedHeadingLabel=Installation complete
FinishedLabelNoIcons=[name] has been successfully installed.
FinishedLabel=
StatusExtractFiles=Extracting...
StatusUninstalling=Removing...
ConfirmUninstall=Are you sure you want to completely remove {#AppName}?
UninstallStatusLabel=
ExitSetupTitle=Exit Installer
ExitSetupMessage=Installation is not complete.%n%nExit {#AppName} Installer?
WizardUninstalling=Uninstalling
ButtonBrowse=&Browse
ButtonWizardBrowse=B&rowse
DirExists=The folder%n%n%1%n%nalready exists. Install to that folder anyway?
DirDoesntExist=The folder%n%n%1%n%ndoes not exist. Create the folder?


[Code]
procedure InitializeWizard();
begin
  WizardForm.Bevel.Visible := False;
  WizardForm.Bevel1.Visible := False;
  WizardForm.Color := clWhite;
  WizardForm.SelectDirBitmapImage.Visible := False;
end;
procedure InitializeUninstallProgressForm();
begin
  UninstallProgressForm.Bevel.Visible := False;
  UninstallProgressForm.Bevel1.Visible := False;
  UninstallProgressForm.Color := clWhite;
end;
// see https://stackoverflow.com/questions/35104265/the-official-inno-setup-code-to-change-next-button-to-install-with-disablereadyp
// we disable DisableProgramGroupPage and DisableDirPage
// this code ensures the button's have the correct caption
procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageID = wpSelectDir then
    WizardForm.NextButton.Caption := SetupMessage(msgButtonInstall)
  else if (CurPageID = wpFinished) then
    WizardForm.NextButton.Caption := SetupMessage(msgButtonFinish)
  else
    WizardForm.NextButton.Caption := SetupMessage(msgButtonNext);
end;


[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{054B4BC6-BD30-45C8-A623-8F5BA6EBD55D}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher=Julia Language
AppPublisherURL=https://julialang.org
AppCopyright=Copyright 2009-{#CurrentYear}; Julia Langage
VersionInfoDescription=Julia Installer
UsePreviousPrivileges=no
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=commandline
WizardStyle=modern
Compression=lzma2/ultra
SolidCompression=yes
SetupIconFile={#AppHomeFiles}\contrib\windows\julia.ico
DefaultDirName={autopf}\{#AppName}\{#AppName}-{#AppVersion}
DefaultGroupName="{#AppName} {#AppVersion}"
UsePreviousGroup=no
UsePreviousAppDir=no
DisableDirPage=no
WizardSizePercent=100
WizardResizable=yes
DisableProgramGroupPage=yes
DisableReadyPage=yes
WizardImageFile={#AppHomeFiles}\contrib\windows\julia-banner.bmp
WizardSmallImageFile={#AppHomeFiles}\contrib\windows\julia-dots.bmp
UninstallDisplayName={#AppName} {#AppVersion}
UninstallDisplayIcon={app}\{#AppMainExe}


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"


[Files]
Source: "{#AppSourceFiles}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs


[Icons]
; create the start menu shortcuts
Name: "{group}\{#AppName} {#AppVersion}"; Filename: "{app}\{#AppMainExe}"; WorkingDir: "{app}"; IconFilename: "{app}\{#AppMainExe}"
Name: "{group}\Uninstall {#AppName} {#AppVersion}"; Filename: "{uninstallexe}"
; create a shortcut in the main installation folder
Name: "{app}\{#AppName}"; Filename: "{app}\{#AppMainExe}"; WorkingDir: "{app}"; IconFilename: "{app}\{#AppMainExe}"


[Run]
Filename: "{app}\{#AppName}"; Description: "Run {#AppName}"; WorkingDir: "{app}"; Flags: nowait postinstall skipifsilent unchecked shellexec
Filename: "{app}"; Description: "Open {#AppName} installation directory"; Flags: nowait postinstall skipifsilent unchecked shellexec
Filename: "https://docs.julialang.org"; Description: "Open documentation"; Flags: nowait postinstall skipifsilent unchecked shellexec
