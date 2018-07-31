; -- Example2.iss --
; Same as Example1.iss, but creates its icon in the Programs folder of the
; Start Menu instead of in a subfolder, and also creates a desktop icon.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

#define AppVersion GetFileVersion(AddBackslash(SourcePath) + "statmon.exe")

[Setup]
AppName=���������: ���������� �������
AppVersion={#AppVersion}
DefaultDirName={sd}\ClassCard\SchoolAgent
DefaultGroupName=���������
Compression=lzma2
SolidCompression=yes
;DisableWelcomePage=yes
;DisableProgramGroupPage=yes
;DisableDirPage=yes
UninstallDisplayIcon={app}\statmon.exe
;OutputDir=userdocs:Inno Setup Examples Output
OutputBaseFilename=SchoolAgent_{#AppVersion}_setup
OutputDir=Installer

[Languages]
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"

;[Types]
;Name: "full"; Description: "��� ����������"
;Name: "compact"; Description: "������� �����������"
;Name: "custom"; Description: "���������� ����������"

;[Components]
;Name: sample_db; Description: "���������������� ���� ������"; Types: "full"
;Name: docs; Description: "����������� ��������������"; Types: "full"

[Dirs]
;Name: "{app}\docs"
Name: "{app}\logs"
;Name: "{app}\logs"; Flags: uninsalwaysuninstall

[Files]
Source: "statmon.exe"; DestDir: "{app}"
;Source: "sqlite3.dll"; DestDir: "{app}"
Source: "conf_models.ini"; DestDir: "{app}"
Source: "changes.txt"; DestDir: "{app}"
;Source: "docs\MealOrder - ����������� ��������������.pdf"; DestDir: "{app}\docs"; Components: docs 
Source: "config_demo.ini"; DestDir: "{app}"; DestName: "config.ini"; Flags: onlyifdoesntexist 
;Source: "db.sqlite3"; DestDir: "{app}"; Flags: onlyifdoesntexist; Components: sample_db 
;Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\���������� �������"; Filename: "{app}\statmon.exe"
;Name: "{group}\����������� �������������� (PDF)"; Filename: "{app}\docs\MealOrder - ����������� ��������������.pdf"
Name: "{group}\������� ���������� �������"; Filename: "{uninstallexe}"
Name: "{commondesktop}\���������� �������"; Filename: "{app}\statmon.exe"

;[INI]
;Filename: "{app}\config.ini"; Section: "Lockers"; Key: "Enabled"; String: "0";
;Filename: "{app}\config.ini"; Section: "Turnstiles"; Key: "Enabled"; String: "0";
;Filename: "{app}\config.ini"; Section: "Lockers"; Key: "Enabled"; String: "1"; Components: lockers
;Filename: "{app}\config.ini"; Section: "Turnstiles"; Key: "Enabled"; String: "1"; Components: turnstiles
