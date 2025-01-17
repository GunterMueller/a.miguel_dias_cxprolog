
; Inno Setup script for building the Windows distribution of CxProlog
;
; Script partially generated by the Inno Setup Script Wizard
; Script manually updated by Paulo Moura e Artur Miguel Dias
;
; Last updated: 30-June-2008

#define MyAppName "CxProlog"
#define MyAppVerName "CxProlog 0.97.1"
#define MyAppURL "http://ctp.di.fct.unl.pt/~amd/cxprolog/"
#define MyAppUrlName "CxProlog Web Site.url"

#define CXPROLOG_DIR "{reg:HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment,CXPROLOG_DIR|{pf}\CxProlog}"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
ChangesEnvironment=yes
DefaultDirName={#CXPROLOG_DIR}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
LicenseFile=C:\cxprolog-0.97.1-windows\COPYING.txt
InfoBeforeFile=C:\cxprolog-0.97.1-windows\INSTALL-windows.txt
OutputBaseFilename=cxprolog-0.97.1-windows
Compression=lzma
SolidCompression=yes
PrivilegesRequired=none

VersionInfoVersion=0.97.1
VersionInfoCopyright=@ Artur Miguel Dias, 2008

AllowRootDirectory=yes
UninstallFilesDir="{userdocs}\CxProlog uninstaller"

MinVersion=0,5.0

[Types]
Name: "cxprolog"; Description: "CxProlog installation"

[Components]
Name: "base"; Description: "CxProlog system"; Types: cxprolog; Flags: disablenouninstallwarning

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Messages]
BeveledLabel=CxProlog 0.97.1 @ Artur Miguel Dias, 2008

[Dirs]
Name: "{userdocs}\CxProlog uninstaller"

[Files]
Source: "C:\cxprolog-0.97.1-windows\*"; Excludes: ".*,CVS"; DestDir: "{app}"; Components: base; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{userdesktop}\CxProlog"; Filename: "{app}\cxprolog.exe"; Parameters: ""; Comment: "Runs CxProlog within a command shell"; WorkingDir: "{userdocs}"; Components: base; Flags: createonlyiffileexists
Name: "{group}\CxProlog"; Filename: "{app}\cxprolog.exe"; Parameters: ""; Comment: "Runs CxProlog within a command shell"; WorkingDir: "{userdocs}"; Components: base; Flags: createonlyiffileexists
Name: "{group}\License"; Filename: "{app}\COPYING.txt"; Components: base
Name: "{group}\Read Me"; Filename: "{app}\README.txt"; Components: base
Name: "{group}\Setup"; Filename: "{app}\INSTALL-windows.txt"; Components: base
Name: "{group}\Web Site"; Filename: "{#MyAppUrl}"; Components: base
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"; Components: base

[Registry]
Root: HKLM; Subkey: "SOFTWARE\CxProlog"; ValueType: expandsz; ValueName: "CXPROLOGDIR"; ValueData: "{app}"; Components: base; Flags: deletevalue uninsdeletevalue

[UninstallDelete]
Type: filesandordirs; Name: "{app}"; Components: base
Type: filesandordirs; Name: "{group}"; Components: base

