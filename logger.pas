{*
Sergey Bodrov (serbod@gmail.com) 2013

TDLogger - write messages to log file and to SysLog server

on Windows, log file closed after 200 milliseconds since last message

*}
unit Logger;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef MSWINDOWS}Windows, Messages, {$endif}
  Classes, SysUtils, blcksock, SyncObjs;

type
  TLogLevel = (
    llEmergency = 0,
    llAlert     = 1,
    llCritical  = 2,
    llError     = 3,
    llWarning   = 4,
    llNotify    = 5,
    llInfo      = 6,
    llDebug     = 7
  );

  TLogRecord = record
    RFC5424: boolean;      // RFC 5424 compilant (new format)
    Facility: byte;        // type of program (0..31) 0..15 is reserved for system
    Severity: byte;        // event severity code  (0..7)
    Timestamp: TDateTime;  // log message timestamp
    Hostname: string;      // source hostname
    IpAddress: string;     // source IP-address
    Appname: string;       // source application name
    ProcID: string;        // source process name inside application
    MsgID: string;         // message code (not used in old format)
    Text: string;          // log message text
  end;

  TWaitStateEvent = procedure(IsWaiting: boolean) of object;
  TLogEvent = procedure(Sender: TObject; const Text: string; LogLevel: TLogLevel; Obj: TObject) of object;

  { TDLogger }

  TDLogger = class (TObject)
  private
    fh: THandle;
    n: Integer;
    FLock: TCriticalSection;
    FActualLogFileName: string;
    FLastMsg: array[TLogLevel] of string;
    FLastMsgCount: array[TLogLevel] of integer;
    FOnLogEvent: TLogEvent;
    FOnLogStatus: TGetStrProc;
    FWaitStateEvent: TWaitStateEvent;
    FSock: TUDPBlockSocket;
    FHostName: string;
    FSyslogServerAddr: string;
    {$ifdef MSWINDOWS}
    {$ifndef FPC}
    FWindowHandle: HWND;
    procedure WndProc(var Msg: TMessage);
    procedure ResetTimer();
    {$endif}
    {$endif}
    function GetActualLogFileName(): string;
    procedure SetSyslogServerAddr(const Value: string);
  public
    ApplicationName: string;
    LogFileName: string;
    TimestampFormat: string;
    RepeatCount: Integer;
    FileCloseDelay: Integer;
    MaxLogLevel: TLogLevel;
    constructor Create(Conf: TStrings);
    destructor Destroy(); override;
    procedure ReadConfig(Conf: TStrings);
    { group similar messages, trigger events, write to file and syslog }
    procedure Log(ll: TLogLevel; const sMsg: string; AObj: TObject = nil);
    { write to file and syslog }
    procedure LogRaw(ll: TLogLevel; const sMsg: string);
    { write to file }
    procedure WriteToFile(ll: TLogLevel; const sMsg: string; dt: TDateTime);
    { write to syslog }
    procedure SendToSyslog(ll: TLogLevel; const sMsg: string; dt: TDateTime; const AProcID: string = '');
    // rotate old logs, .log renamed to .l01, .l01 -> .l02 and so on..
    procedure Rotate(MaxLogFiles: Integer = 3);
    property ActualLogFileName: string read GetActualLogFileName;
    property HostName: string read FHostName write FHostName;
    property SyslogServerAddr: string read FSyslogServerAddr write SetSyslogServerAddr;
    property OnLogEvent: TLogEvent read FOnLogEvent write FOnLogEvent;
    property OnLogStatus: TGetStrProc read FOnLogStatus write FOnLogStatus;
    property OnWaitState: TWaitStateEvent read FWaitStateEvent write FWaitStateEvent;
  end;

function DLogger : TDLogger;
procedure _Log(const AMsg: string; ALogLevel: TLogLevel = llInfo; AObj: TObject = nil);
procedure _LogDebug(const AMsg: string; AObj: TObject = nil);
procedure _LogError(const AMsg: string; AObj: TObject = nil);
procedure _LogWarning(const AMsg: string; AObj: TObject = nil);
procedure _LogStatus(const AMsg: string);
procedure _WaitState(IsWaiting: boolean);
function LogRecToSyslogStr(const rec: TLogRecord; var ASyslogStr: AnsiString): Boolean;
procedure _LogProfiler(const AMsg: string);
{ returns info about current exception }
function GetExceptionMessage(): string;

implementation

{$ifndef FPC}
const
  fsFromEnd = 2;
{$endif}

var
  LoggerObject: TDLogger;
  DProfilerLastTime: Cardinal;

// -----------------------------------------------------------------------------
function DLogger : TDLogger;
begin
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  Result := LoggerObject;
end;

procedure _Log(const AMsg: string; ALogLevel: TLogLevel = llInfo; AObj: TObject = nil);
begin
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  LoggerObject.log(ALogLevel, AMsg, AObj);
end;

procedure _LogDebug(const AMsg: string; AObj: TObject = nil);
begin
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  LoggerObject.log(llDebug, AMsg, AObj);
end;

procedure _LogError(const AMsg: string; AObj: TObject = nil);
begin
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  LoggerObject.log(llError, AMsg, AObj);
end;

procedure _LogWarning(const AMsg: string; AObj: TObject = nil);
begin
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  LoggerObject.log(llWarning, AMsg, AObj);
end;

procedure _LogStatus(const AMsg: string);
begin
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  if Assigned(LoggerObject.OnLogStatus) then
    LoggerObject.OnLogStatus(AMsg);
end;

procedure _WaitState(IsWaiting: boolean);
begin
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  if Assigned(LoggerObject.OnWaitState) then
    LoggerObject.OnWaitState(IsWaiting);
end;

procedure _LogProfiler(const AMsg: string);
var
  tc: Cardinal;
begin
  tc := GetTickCount();
  if not Assigned(LoggerObject) then
    LoggerObject := TDlogger.Create(nil);
  LoggerObject.SendToSyslog(llDebug, IntToStr(tc-DProfilerLastTime)+' '+AMsg, 0, 'PROF');
  DProfilerLastTime := tc;
end;

function GetExceptionMessage(): string;
var
  E: Exception;
begin
  E := Exception(ExceptObject);
  if Assigned(E) then
  begin
    {$ifdef FPC}
    Result := E.ClassName+'($'+IntToHex(PtrUInt(ExceptAddr), 8)+'): '+E.Message;
    {$else}
    Result := E.ClassName+'($'+IntToHex(Cardinal(ExceptAddr), 8)+'): '+E.Message;
    {$endif}
  end
  else
    Result := '';
end;

function LLToInt(ll: TLogLevel): integer;
begin
  case ll of
    llEmergency: Result := 0;
    llAlert:     Result := 1;
    llCritical:  Result := 2;
    llError:     Result := 3;
    llWarning:   Result := 4;
    llNotify:    Result := 5;
    llInfo:      Result := 6;
    llDebug:     Result := 7;
  else
    Result := 6;
  end;
end;

function LLToStr(ll: TLogLevel): string;
begin
  case ll of
    llEmergency: Result := 'EMERG';
    llAlert:     Result := 'ALERT';
    llCritical:  Result := 'CRIT ';
    llError:     Result := 'ERROR';
    llWarning:   Result := 'WARN ';
    llNotify:    Result := 'NOTE ';
    llInfo:      Result := 'INFO ';
    llDebug:     Result := 'DEBUG';
  else
    Result := 'INFO ';
  end;
end;

function LogRecToSyslogStr(const rec: TLogRecord; var ASyslogStr: AnsiString): Boolean;
var
  i: Integer;
  y, m, d: Word;
  s, sm: string;

function Wrap(const text: string): string;
begin
  Result := Trim(text);
  if Result = '' then
    Result := '-';
end;

begin
  Result := True;
  i := (rec.Facility * 8) + rec.Severity;
  if rec.RFC5424 then  // syslog format (RFC 5424)
  begin
    ASyslogStr := '<' + IntToStr(i) + '>1'  // PRIVAL and VERSION
    +' '+FormatDateTime('YYYY-MM-DDTHH:MM:SS', rec.Timestamp) // TIMESTAMP
    +' '+Wrap(rec.Hostname) // HOSTNAME
    +' '+Wrap(rec.Appname)  // APPNAME
    +' '+Wrap(rec.ProcID)   // PROCID
    +' '+Wrap(rec.MsgID)    // MSGID
    +' '+Wrap('')           // STRUCTURED-DATA
    +' '+Wrap(rec.Text);    // MSG
  end
  else  // BSD format (RFC 3164)
  begin
    DecodeDate(rec.Timestamp, y, m, d);
    s := 'Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec';
    sm := Copy(s, ((m-1)*5)+1, 3);

    ASyslogStr := '<'+IntToStr(i)+'>'  // PRIVAL
      +sm+' '+FormatDateTime('DD HH:MM:SS', rec.Timestamp) // TIMESTAMP
      +' '+Wrap(rec.Hostname); // HOSTNAME
    if Length(rec.Appname) > 0 then
    begin
      s := s+' '+Wrap(rec.Appname);  // APPNAME
      if rec.ProcID <> '' then
      begin
        s := s+'['+Wrap(rec.ProcID)+']';   // PROCID
      end;
      s := s + ':';
    end;
    s := s+' '+Wrap(rec.Text);    // MSG
  end;
end;


{ TDLogger }

constructor TDLogger.Create(Conf: TStrings);
begin
  inherited Create();
  FSock := nil;
  FLock := TCriticalSection.Create();
  Self.MaxLogLevel := llDebug;
  Self.ApplicationName := ExtractFileName(ParamStr(0));
  Self.LogFileName := self.ApplicationName+'.log';
  Self.TimestampFormat := 'DD-MM-YYYY HH:NN:SS.ZZZ';
  Self.SyslogServerAddr := '';
  Self.HostName := '';
  Self.RepeatCount := 50;
  Self.FileCloseDelay := 200;
  n := 0;
  ReadConfig(Conf);
  {$ifdef MSWINDOWS}
  {$ifndef FPC}
  FWindowHandle := Classes.AllocateHWnd(WndProc);
  {$endif}
  {$endif}
end;

destructor TDLogger.Destroy();
begin
  FreeAndNil(FLock);
  {$ifdef MSWINDOWS}
  {$ifndef FPC}
  Classes.DeallocateHWnd(FWindowHandle);
  {$endif}
  {$endif}
  if n > 0 then
  begin
    n := 0;
    FileClose(fh);
  end;
  if Assigned(FSock) then
    FSock.Free();
  inherited Destroy();
end;

procedure TDLogger.ReadConfig(Conf: TStrings);
var
  i, iLogLev: Integer;
  sName: string;
begin
  if Assigned(Conf) then
  begin
    // names can be in mixed-case letters
    Conf.NameValueSeparator := '=';
    for i:=0 to Conf.Count-1 do
    begin
      sName := UpperCase(Conf.Names[i]);
      if sName = 'FORMAT' then self.TimestampFormat := Conf.ValueFromIndex[i]
      else if sName = 'FILE' then self.LogFileName := Conf.ValueFromIndex[i]
      else if sName = 'SYSLOG' then self.SyslogServerAddr := Conf.ValueFromIndex[i]
      else if sName = 'APPNAME' then self.ApplicationName := Conf.ValueFromIndex[i]
      else if sName = 'HOSTNAME' then self.HostName := Conf.ValueFromIndex[i]
      else if sName = 'LOGLEVEL' then
      begin
        iLogLev := StrToIntDef(Conf.ValueFromIndex[i], Ord(Self.MaxLogLevel));
        if (iLogLev <= Ord(High(TLogLevel))) and (iLogLev >= Ord(Low(TLogLevel))) then
          Self.MaxLogLevel := TLogLevel(iLogLev);
      end;
    end;
  end;
end;

procedure TDLogger.WriteToFile(ll: TLogLevel; const sMsg: string; dt: TDateTime);
var
  {$IFDEF FPC}
  Res: THandle;
  {$ELSE}
  Res: Integer;
  {$ENDIF}
  s: string;
begin
  if n = 0 then
  begin
    FActualLogFileName := self.LogFileName+FormatDateTime('-YYYY-MM-DD', dt) + '.txt';
    {if not FileExists(FActualLogFileName) then
    begin
      // Just created file not shared, despite to access flags
      Res := FileCreate(FActualLogFileName, 438);
      if Res > 0 then
      begin
        fh := Res;
        FileClose(fh);
      end;
    end;}
    Res := FileOpen(FActualLogFileName, (fmOpenReadWrite or fmShareDenyNone));
    {$IFDEF FPC}
    if Res <> feInvalidHandle then
    {$ELSE}
    if Res > 0 then
    {$ENDIF}
    begin
      fh := Res;
      FileSeek(fh, 0, fsFromEnd);
    end
    else
    begin
      // file not exists? try to create new
      // Just created file not shared, despite to access flags
      Res := FileCreate(FActualLogFileName, 438);
      {$IFDEF FPC}
      if Res <> feInvalidHandle then
      {$ELSE}
      if Res > 0 then
      {$ENDIF}
      begin
        FileClose(Res);
        Res := FileOpen(FActualLogFileName, (fmOpenReadWrite or fmShareDenyNone));
        {$IFDEF FPC}
        if Res = feInvalidHandle then
        {$ELSE}
        if Res <= 0 then
        {$ENDIF}
          Exit;
        fh := Res;
      end
      else
        Exit;
    end;
  end;
  s := FormatDateTime(TimestampFormat, dt)+' '+LLToStr(ll)+' '+sMsg+CRLF;
  FileWrite(fh, s[1], Length(s));
  //Flush(f); // forced flush data from buffer to file
  inc(n);
  if n > 10 then
  begin
    // another forced flush
    n := 0;
    //CloseFile(f);
    FileClose(fh);
  end;
end;

{*
procedure TDLogger.write2(ll: TLogLevel; sMsg: string);
var
  sFileName: string;
begin
  while lock do Sleep(1);
  lock:=True;
  if n=0 then
  begin
    sFileName:=self.LogFileName+FormatDateTime('-YYYY-MM-DD', Now) + '.txt';
    AssignFile(f, sFileName);
    if FileExists(sFileName) then Append(f) else Rewrite(f);
  end;
  Writeln(f, LLToStr(ll)+' | '+FormatDateTime(TimestampFormat, Now) + ' | '+sMsg);
  Flush(f); // forced flush data from buffer to file
  inc(n);
  if n > 10 then
  begin
    // another forced flush
    n:=0;
    CloseFile(f);
  end;
  if self.SyslogServerAddr<>'' then
  begin
    //slogsend.ToSysLog(self.SyslogServerAddr, 22, LLToSLS(ll), sMsg);
    Self.send(ll, sMsg);
  end;
  lock:=False;
end;
*}

procedure TDLogger.SetSyslogServerAddr(const Value: string);
begin
  FSyslogServerAddr := Value;
  if Assigned(FSock) then
    FreeAndNil(FSock);
end;

procedure TDLogger.SendToSyslog(ll: TLogLevel; const sMsg: string; dt: TDateTime; const AProcID: string = '');
var
  //Sock: TUDPBlockSocket;
  rec: TLogRecord;
  s: AnsiString;
begin
  if not Assigned(FSock) then
  begin
    FSock := TUDPBlockSocket.Create();
    if FSyslogServerAddr = '' then
      FSyslogServerAddr := 'localhost';
    FSock.Connect(FSyslogServerAddr, '514');
    if FHostName = '' then
      FHostName := FSock.Localname;
  end;

  rec.Facility  := 16;
  rec.Severity  := LLToInt(ll);
  rec.Timestamp := dt;
  rec.Hostname  := self.HostName;
  rec.Appname   := self.ApplicationName;
  rec.ProcID    := AProcID;
  rec.MsgID     := '';
  rec.Text      := sMsg;

  try
    s := '';
    LogRecToSyslogStr(rec, s);
    FSock.SendString(s);
  except
    FreeAndNil(FSock);
  end;
end;


procedure TDLogger.LogRaw(ll: TLogLevel; const sMsg: string);
var
  dt: TDateTime;
begin
  FLock.Acquire();
  try
    try
      dt := Now();
      // send to syslog server
      if Length(Self.SyslogServerAddr) > 0 then
      begin
        //slogsend.ToSysLog(self.SyslogServerAddr, 22, LLToSLS(ll), sMsg);
        Self.SendToSyslog(ll, sMsg, dt);
      end;

      // write to log file
      if Length(Self.LogFileName) > 0 then
      begin
        Self.WriteToFile(ll, sMsg, dt);
      end;

    except
      // nothing... catch exceptions here and don't send it anywhere
    end;
  finally
    FLock.Release();
  end;

end;

procedure TDLogger.Log(ll: TLogLevel; const sMsg: string; AObj: TObject = nil);
var
  s: string;
begin
  if Assigned(OnLogEvent) then OnLogEvent(Self, sMsg, ll, AObj);

  if ll > Self.MaxLogLevel then Exit;

  s := Trim(sMsg);
  if RepeatCount > 0 then
  begin
    if FLastMsg[ll] = s then
    begin
      Inc(FLastMsgCount[ll]);
      if FLastMsgCount[ll] < RepeatCount then Exit;
    end;
    if FLastMsgCount[ll] > 1 then
    begin
      Self.LogRaw(ll, FLastMsg[ll]+' (x'+IntToStr(FLastMsgCount[ll])+')');
      FLastMsgCount[ll]:=0;
    end;
    FLastMsg[ll] := s;
  end;
  Self.LogRaw(ll, s);
  {$ifdef MSWINDOWS}
  {$ifndef FPC}
  ResetTimer();
  {$endif}
  {$endif}
end;

procedure TDLogger.Rotate(MaxLogFiles: Integer);
var
  i: Integer;
  TmpName, NewName: String;
  LogExists: Boolean;
begin
  LogExists := FileExists(ActualLogFileName);
  // rotate old logs
  if LogExists then
  begin
    NewName := ChangeFileExt(ActualLogFileName, Format('.l%2.2d', [MaxLogFiles]));
    if (FileExists(NewName))then
      DeleteFile(NewName);
    for i := MaxLogFiles-1 downto 1 do
    begin
      TmpName := ChangeFileExt(ActualLogFileName, Format('.l%2.2d', [i]));
      if FileExists(TmpName) then
        RenameFile(TmpName, NewName);
      NewName := TmpName;
    end;
    if FileExists(ActualLogFileName) then
      RenameFile(ActualLogFileName, NewName);
  end;
  //LogFileName := FileName;
end;

function TDLogger.GetActualLogFileName(): string;
begin
  Result := FActualLogFileName;
  if Length(Trim(Result)) = 0 then
    Result := self.LogFileName+FormatDateTime('-YYYY-MM-DD', Now) + '.txt';
end;

{$ifdef MSWINDOWS}
{$ifndef FPC}
procedure TDLogger.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_TIMER then
  begin
    FLock.Acquire();
    try
      if n > 0 then
      begin
        // another forced flush
        n := 0;
        FileClose(fh);
      end;

      if Assigned(FSock) then
        FreeAndNil(FSock);
    finally
      FLock.Release();
    end;
    KillTimer(FWindowHandle, 1);
  end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TDLogger.ResetTimer();
begin
  KillTimer(FWindowHandle, 1);
  if FileCloseDelay > 0 then
  begin
    if SetTimer(FWindowHandle, 1, FileCloseDelay, nil) = 0 then
      raise EOutOfResources.Create('Not enough timers available');
  end;
end;
{$endif}
{$endif}

initialization

LoggerObject := nil;

finalization

if Assigned(LoggerObject) then
  FreeAndNil(LoggerObject);

end.

