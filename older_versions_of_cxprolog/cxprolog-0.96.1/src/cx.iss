; Script generated by the Inno Setup Script Wizard.

#define MyAppName "CxProlog"
#define MyAppVerName "CxProlog 0.96.1"
#define MyAppPublisher "Artur Miguel Dias"
#define MyAppURL "http://ctp.di.fct.unl.pt/~amd/cxprolog/"
#define MyAppUrlName "CxProlog Web Site.url"

#define CXPROLOG_DIR "{reg:HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment,CXPROLOG_DIR|{pf}\CxProlog}"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
ChangesEnvironment=yes
DefaultDirName={#CXPROLOG_DIR}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
LicenseFile=C:\cxprolog-0.96.1-windows\COPYING
InfoBeforeFile=C:\cxprolog-0.96.1-windows\README
OutputBaseFilename=cxprolog-0.96.1
Compression=lzma
SolidCompression=yes
PrivilegesRequired=none

VersionInfoVersion=0.96.1
VersionInfoCopyright=@ Artur Miguel Dias, 2007

AllowRootDirectory=yes
UninstallFilesDir="{userdocs}\CxProlog uninstaller"

MinVersion=0,5.0

[Types]
Name: "full"; Description: "Full CxProlog installation"

[Components]
Name: "base"; Description: "Base system"; Types: full; Flags: disablenouninstallwarning

[Tasks]
Name: website; Description: "&Visit {#MyAppName} web site"; Components: base
Name: shortcut; Description: "&Create a desktop shortcut to the CxProlog folder"; Components: base

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Messages]
BeveledLabel=CxProlog 0.96.1 @ Artur Miguel Dias, 2007

[Dirs]
Name: "{userdocs}\CxProlog uninstaller"

[Files]
Source: "C:\cxprolog-0.96.1-windows\*"; Excludes: ".*,CVS"; DestDir: "{app}"; Components: base; Flags: ignoreversion recursesubdirs createallsubdirs

[INI]
Filename: "{app}\{#MyAppUrlName}"; Section: "InternetShortcut"; Key: "URL"; String: "{#MyAppURL}"; Components: base

[Icons]
Name: "{group}\CxProlog"; Filename: "{app}\cxprolog.exe"; Parameters: ""; Comment: "Runs CxProlog within a command shell"; WorkingDir: "{userdocs}"; Components: base; Flags: createonlyiffileexists

Name: "{group}\License"; Filename: "{app}\COPYING"; Components: base
Name: "{group}\Read Me"; Filename: "{app}\README"; Components: base

Name: "{group}\Web Site"; Filename: "{#MyAppUrl}"; Components: base

Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"; Components: base

[Registry]
Root: HKLM; Subkey: "SOFTWARE\CxProlog"; ValueType: expandsz; ValueName: "CXPROLOGDIR"; ValueData: "{app}"; Components: base; Flags: deletevalue uninsdeletevalue

[Run]
Filename: "{app}\{#MyAppUrlName}"; Flags: shellexec nowait; Tasks: website

[UninstallDelete]
Type: filesandordirs; Name: "{app}"; Components: base
Type: filesandordirs; Name: "{group}"; Components: base
