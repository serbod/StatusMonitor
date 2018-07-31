{
Includes vinfo.pas from xplproject
}

unit sysinfo;

{$mode objfpc}

interface

uses
  Classes, SysUtils, resource, versiontypes, versionresource, process;

type
  { TVersionInfo }

  TVersionInfo = class
  private
    FVersResource: TVersionResource;
    function GetFixedInfo: TVersionFixedInfo;
    function GetStringFileInfo: TVersionStringFileInfo;
    function GetVarFileInfo: TVersionVarFileInfo;

    function SearchValue(const aString: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function CompanyName: string;
    function InternalName: string;
    function FileVersion: string;

    procedure Load(Instance: THandle);
    property FixedInfo: TVersionFixedInfo read GetFixedInfo;
    property StringFileInfo: TVersionStringFileInfo read GetStringFileInfo;
    property VarFileInfo: TVersionVarFileInfo read GetVarFileInfo;
  end;

{ exported functions }
function GetIpAddrList(): string;
function GetVersion(): string;
procedure UpdateVersionInfo();


implementation

{ TVersionInfo }

function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
  Result := FVersResource.FixedInfo;
end;

function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
  Result := FVersResource.StringFileInfo;
end;

function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
  Result := FVersResource.VarFileInfo;
end;

function TVersionInfo.SearchValue(const aString: string): string;
var
  s: TVersionStringTable;
  i, j: integer;
begin
  Result := '';
  for i := 0 to StringFileInfo.Count - 1 do
  begin
    s := StringFileInfo.Items[i];
    for j := 0 to s.Count - 1 do
      if s.Keys[j] = aString then
      begin
        Result := s.Values[j];
        break;
      end;
  end;
end;

function TVersionInfo.CompanyName: string;
begin
  Result := SearchValue('CompanyName');
end;

function TVersionInfo.InternalName: string;
begin
  Result := SearchValue('InternalName');
end;

function TVersionInfo.FileVersion: string;
begin
  Result := SearchValue('FileVersion');
end;

constructor TVersionInfo.Create;
begin
  inherited Create;
  FVersResource := TVersionResource.Create;
  Load(HInstance);
end;

destructor TVersionInfo.Destroy;
begin
  FVersResource.Free;
  inherited Destroy;
end;

procedure TVersionInfo.Load(Instance: THandle);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, 1, PChar(RT_VERSION));
  try
    FVersResource.SetCustomRawDataStream(Stream);
    // access some property to load from the stream
    FVersResource.FixedInfo;
    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);
  finally
    Stream.Free;
  end;
end;

function GetIpAddrList(): string;
var
  AProcess: TProcess;
  s: string;
  sl: TStringList;
  i, n: integer;

begin
  Result := '';
  sl := TStringList.Create();
  try
    {$IFDEF WINDOWS}
    AProcess := TProcess.Create(nil);
    try
      AProcess.CommandLine := 'ipconfig.exe';
      AProcess.Options := AProcess.Options + [poUsePipes, poNoConsole];
      AProcess.Execute();
      Sleep(500); // poWaitOnExit not works as expected
      sl.LoadFromStream(AProcess.Output);
    finally
      AProcess.Free();
    end;
    //ExecuteProcess('', 'ipconfig.exe > ipconfig.log');
    //ExecuteProcess('', 'ipconfig.exe', []);
    //ShellExecute('ipconfig > ipconfig.log');
    //sl.LoadFromFile('ipconfig.log');
    for i := 0 to sl.Count - 1 do
    begin
      if (Pos('IPv4', sl[i]) = 0) and (Pos('IP-', sl[i]) = 0) and
        (Pos('IP Address', sl[i]) = 0) then
        Continue;
      s := sl[i];
      s := Trim(Copy(s, Pos(':', s) + 1, 999));
      if Pos(':', s) > 0 then
        Continue; // IPv6
      Result := Result + s + '  ';
    end;
    {$ENDIF}
    {$IFDEF UNIX}
    //ExecuteProcess('/bin/', 'ifconfig > ifconfig.log');
    //sl.LoadFromFile('ifconfig.log');
    AProcess := TProcess.Create(nil);
    AProcess.CommandLine := '/sbin/ifconfig';
    AProcess.Options := AProcess.Options + [poUsePipes, poWaitOnExit];
    try
      AProcess.Execute();
      //Sleep(500); // poWaitOnExit не работает как ожидалось
      sl.LoadFromStream(AProcess.Output);
    finally
      AProcess.Free();
    end;

    for i := 0 to sl.Count - 1 do
    begin
      n := Pos('inet addr:', sl[i]);
      if n = 0 then
        Continue;
      s := sl[i];
      s := Copy(s, n + Length('inet addr:'), 999);
      Result := Result + Trim(Copy(s, 1, Pos(' ', s))) + '  ';
    end;
    {$ENDIF}
  finally
    sl.Free();
  end;
end;

function GetVersion(): string;
var
  Info: TVersionInfo;
begin
  Info := TVersionInfo.Create();
  try
    Info.Load(HINSTANCE);
    Result := IntToStr(Info.FixedInfo.FileVersion[1]) + '.' + IntToStr(
      Info.FixedInfo.FileVersion[2]) + '.' + IntToStr(Info.FixedInfo.FileVersion[3]);
  finally
    Info.Free;
  end;
end;

procedure UpdateVersionInfo();
var
  s, sFileName: string;
  fh: THandle;
begin
  //{$I %DATE%};
  s := GetVersion();

  sFileName := 'VERSION';
  if not FileExists(sFileName) then
  begin
    // Just created file not shared, despite to access flags
    fh := FileCreate(sFileName, 438);
  end
  else
  begin
    fh := FileOpen(sFileName, (fmOpenWrite or fmShareDenyNone));
  end;
  //FileSeek(fh, 0, fsFromBeginning);
  FileWrite(fh, s[1], Length(s));
  FileClose(fh);
end;


end.
