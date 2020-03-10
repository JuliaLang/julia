#define AppName "Julia"
#define AppNameLong AppName + " " + AppVersion
#define AppMainExeName "bin\julia.exe"
#define CurrentYear GetDateTimeString('yyyy', '', '')
#define DirName AppName + " " + AppVersion


[LangOptions]
DialogFontName=Segoe UI
WelcomeFontName=Segoe UI
CopyrightFontName=Segoe UI
TitleFontName=Segoe UI


[Messages]
SetupAppTitle={#AppName} Installer
SetupWindowTitle=Installer - {#AppNameLong}
UninstallAppTitle={#AppName} Uninstaller
UninstallAppFullTitle=Uninstaller - {#AppNameLong}
WizardSelectDir=Select Installation Directory
SelectDirDesc=
SelectDirLabel3=
SelectDirBrowseLabel=Restart installer as Administrator to install {#AppName} system wide.%n%n%nInstallation directory:
WizardPreparing=Installing
PreparingDesc=
InstallingLabel=
ClickFinish=
FinishedHeadingLabel=Installation Successfull
FinishedLabelNoIcons=[name] has been successfully installed.
FinishedLabel=[name] has been successfully installed.
StatusExtractFiles=Extracting...
StatusUninstalling=Removing...
ConfirmUninstall=Are you sure you want to completely remove {#AppName}?
UninstallStatusLabel=
ExitSetupTitle=Exit Installer
ExitSetupMessage=Installation is not complete.%n%nExit {#AppName} Installer?
WizardUninstalling=Uninstalling
ButtonBrowse=&Browse
ButtonWizardBrowse=B&rowse
DirExists=The selected folder already exists: %n%n%1%n%nInstall anyways?
DirDoesntExist=The folder%n%n%1%n%ndoes not exist. Create the folder?
SelectTasksLabel2=Select additional tasks to perform:
WizardSelectTasks=Select Additional Tasks
SelectTasksDesc=


[Setup]
AppId={{054B4BC6-BD30-45C8-A623-8F5BA6EBD55D}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher=Julia Language
AppPublisherURL=https://julialang.org
AppCopyright=Copyright 2009-{#CurrentYear}; Julia Langage
VersionInfoDescription=Julia Installer
PrivilegesRequiredOverridesAllowed=commandline
WizardStyle=modern
Compression=lzma2/ultra
SolidCompression=yes
DefaultDirName={autopf}\{#DirName}
UsePreviousPrivileges=no
PrivilegesRequired=lowest
DefaultGroupName="{#AppNameLong}"
UsePreviousGroup=no
UsePreviousAppDir=no
DisableDirPage=no
WizardSizePercent=100
WizardResizable=yes
DisableProgramGroupPage=yes
DisableReadyPage=yes
WizardImageFile={#RepoDir}\contrib\windows\julia-banner.bmp
WizardSmallImageFile={#RepoDir}\contrib\windows\julia-dots.bmp
SetupIconFile={#RepoDir}\contrib\windows\julia.ico
UninstallDisplayName={#AppNameLong}
UninstallDisplayIcon={app}\{#AppMainExeName}
UninstallFilesDir={app}\uninstall


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"


[Tasks]
Name: "desktopicon"; Description: "Create a Desktop shortcut"
Name: "startmenu"; Description: "Create a Start Menu entry"


[Files]
Source: "{#SourceDir}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs


[Icons]
Name: "{autostartmenu}\{#AppNameLong}"; Filename: "{app}\{#AppMainExeName}"; WorkingDir: "{%USERPROFILE}"; Tasks: startmenu
Name: "{autodesktop}\{#AppNameLong}"; Filename: "{app}\{#AppMainExeName}"; WorkingDir: "{%USERPROFILE}"; Tasks: desktopicon


[Run]
Filename: "{app}\{#AppMainExeName}"; Description: "Run {#AppName}"; WorkingDir: "{%USERPROFILE}"; Flags: nowait postinstall skipifsilent unchecked shellexec
Filename: "{app}"; Description: "Open {#AppName} directory"; Flags: nowait postinstall skipifsilent unchecked shellexec
Filename: "https://docs.julialang.org"; Description: "Open documentation"; Flags: nowait postinstall skipifsilent unchecked shellexec


[Code]

procedure InitializeWizard;
begin
  WizardForm.Bevel.Visible := False;
  WizardForm.Bevel1.Visible := False;
  WizardForm.SelectDirBitmapImage.Visible := False;

  WizardForm.Color := clWhite;
  WizardForm.MainPanel.Color := WizardForm.Color;
  WizardForm.InnerPage.Color := WizardForm.Color;
  WizardForm.TasksList.Color := WizardForm.Color;
  WizardForm.ReadyMemo.Color := WizardForm.Color;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  case CurPageID of
    wpWelcome: WizardForm.Color := WizardForm.WelcomePage.Color;
    wpFinished: WizardForm.Color := WizardForm.FinishedPage.Color;
  else
    WizardForm.Color := WizardForm.InnerPage.Color;
  end;
end;

procedure InitializeUninstallProgressForm();
begin
  UninstallProgressForm.Color := clWhite;
  UninstallProgressForm.Bevel.Visible := False;
  UninstallProgressForm.Bevel1.Visible := False;
end;
