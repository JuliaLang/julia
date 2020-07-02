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
ChangesEnvironment=true


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[CustomMessages]
AdditionalIcons=Icons:
Other=%nOther:

[Tasks]
Name: "desktopicon"; Description: "Create a Desktop shortcut"; GroupDescription: "{cm:AdditionalIcons}";
Name: "startmenu"; Description: "Create a Start Menu entry"; GroupDescription: "{cm:AdditionalIcons}";
Name: "addtopath"; Description: "Add {#AppName} to PATH"; GroupDescription: "{cm:Other}"; Flags: unchecked;


[Files]
Source: "{#SourceDir}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs


[Icons]
Name: "{autodesktop}\{#AppNameLong}"; Filename: "{app}\{#AppMainExeName}"; WorkingDir: "{%USERPROFILE}"; Tasks: desktopicon
Name: "{autostartmenu}\{#AppNameLong}"; Filename: "{app}\{#AppMainExeName}"; WorkingDir: "{%USERPROFILE}"; Tasks: startmenu


[Run]
Filename: "{app}\{#AppMainExeName}"; Description: "Run {#AppName}"; WorkingDir: "{%USERPROFILE}"; Flags: nowait postinstall skipifsilent unchecked shellexec
Filename: "{app}"; Description: "Open {#AppName} directory"; Flags: nowait postinstall skipifsilent unchecked shellexec
Filename: "https://docs.julialang.org"; Description: "Open documentation"; Flags: nowait postinstall skipifsilent unchecked shellexec


[Registry]
Root: HKA; Subkey: "{code:GetEnvironmentKey}"; ValueType: expandsz; ValueName: "Path"; ValueData: "{olddata};{app}\bin"; Tasks: addtopath; Check: NeedsAddPath(ExpandConstant('{app}\bin'))

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

function GetEnvironmentKey(Param: string): string;
begin
  if IsAdminInstallMode then
    Result := 'System\CurrentControlSet\Control\Session Manager\Environment'
  else
    Result := 'Environment';
end;

// https://stackoverflow.com/a/23838239/261019
procedure Explode(var Dest: TArrayOfString; Text: String; Separator: String);
var
  i, p: Integer;
begin
  i := 0;
  repeat
    SetArrayLength(Dest, i+1);
    p := Pos(Separator,Text);
    if p > 0 then begin
      Dest[i] := Copy(Text, 1, p-1);
      Text := Copy(Text, p + Length(Separator), Length(Text));
      i := i + 1;
    end else begin
      Dest[i] := Text;
      Text := '';
    end;
  until Length(Text)=0;
end;

// https://stackoverflow.com/questions/3304463/how-do-i-modify-the-path-environment-variable-when-running-an-inno-setup-install
function NeedsAddPath(Param: string): boolean;
var
  OrigPath: string;
begin
  if not RegQueryStringValue(HKA, GetEnvironmentKey(''), 'Path', OrigPath)
  then begin
    Result := True;
    exit;
  end;
  Result := Pos(';' + Param + ';', ';' + OrigPath + ';') = 0;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Path: string;
  ExePath: string;
  Parts: TArrayOfString;
  NewPath: string;
  i: Integer;
begin
  if not CurUninstallStep = usUninstall then begin
    exit;
  end;
  if not RegQueryStringValue(HKA, GetEnvironmentKey(''), 'Path', Path)
  then begin
    exit;
  end;
  NewPath := '';
  ExePath := ExpandConstant('{app}\bin')
  Explode(Parts, Path, ';');
  for i:=0 to GetArrayLength(Parts)-1 do begin
    if CompareText(Parts[i], ExePath) <> 0 then begin
      NewPath := NewPath + Parts[i];
      if i < GetArrayLength(Parts) - 1 then begin
        NewPath := NewPath + ';';
      end;
    end;
  end;
  RegWriteExpandStringValue(HKA, GetEnvironmentKey(''), 'Path', NewPath);
end;
