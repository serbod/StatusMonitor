; -- Example2.iss --
; Same as Example1.iss, but creates its icon in the Programs folder of the
; Start Menu instead of in a subfolder, and also creates a desktop icon.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

#define AppVersion GetFileVersion(AddBackslash(SourcePath) + "statmon.exe")

[Setup]
AppName=Класскарт: Мониторинг системы
AppVersion={#AppVersion}
DefaultDirName={sd}\ClassCard\SchoolAgent
DefaultGroupName=Класскарт
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
;Name: "full"; Description: "Все компоненты"
;Name: "compact"; Description: "Минимум компонентов"
;Name: "custom"; Description: "Выборочные компоненты"

;[Components]
;Name: sample_db; Description: "Демонстрационная база данных"; Types: "full"
;Name: docs; Description: "Руководство администратора"; Types: "full"

[Dirs]
;Name: "{app}\docs"
Name: "{app}\logs"
;Name: "{app}\logs"; Flags: uninsalwaysuninstall

[Files]
Source: "statmon.exe"; DestDir: "{app}"
;Source: "sqlite3.dll"; DestDir: "{app}"
Source: "conf_models.ini"; DestDir: "{app}"
Source: "changes.txt"; DestDir: "{app}"
;Source: "docs\MealOrder - руководство администратора.pdf"; DestDir: "{app}\docs"; Components: docs 
Source: "config_demo.ini"; DestDir: "{app}"; DestName: "config.ini"; Flags: onlyifdoesntexist 
;Source: "db.sqlite3"; DestDir: "{app}"; Flags: onlyifdoesntexist; Components: sample_db 
;Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\Мониторинг системы"; Filename: "{app}\statmon.exe"
;Name: "{group}\Руководство администратора (PDF)"; Filename: "{app}\docs\MealOrder - руководство администратора.pdf"
Name: "{group}\Удалить Мониторинг системы"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Мониторинг системы"; Filename: "{app}\statmon.exe"

;[INI]
;Filename: "{app}\config.ini"; Section: "Lockers"; Key: "Enabled"; String: "0";
;Filename: "{app}\config.ini"; Section: "Turnstiles"; Key: "Enabled"; String: "0";
;Filename: "{app}\config.ini"; Section: "Lockers"; Key: "Enabled"; String: "1"; Components: lockers
;Filename: "{app}\config.ini"; Section: "Turnstiles"; Key: "Enabled"; String: "1"; Components: turnstiles
