unit log_service;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, blcksock, Logger;

type
  TSyslogEvent = procedure(const Msg: string; const IPAddr: string) of object;

function StrToLogRec(const Msg: string): TLogRecord;
function SeverityStr(const rec: TLogRecord): string;

type

  ILogItem = interface
    procedure SetLogRec(const ALogRec: TLogRecord);
    function GetLogRec(var ALogRec: TLogRecord): Boolean;
    function GetSeverity(): Byte;
    function GetSeverityStr(): string;
    function ToStr(): string;
  end;

  { TLogItem }

  TLogItem = class(TInterfacedObject, ILogItem)
  private
    FLogRec: TLogRecord;
  public
    procedure SetLogRec(const ALogRec: TLogRecord);
    function GetLogRec(var ALogRec: TLogRecord): Boolean;
    function GetSeverity(): Byte;
    function GetSeverityStr(): string;
    function ToStr(): string;
  end;

  { TLogItemList }

  TLogItemList = class(TInterfaceList)
  public
    MaxItems: Integer;
    procedure AddLogRec(const ALogRec: TLogRecord);
    function GetLogRec(Index: integer; var ALogRec: TLogRecord): Boolean;
  end;


  TLogRecordArray = array of TLogRecord;

  { TSysLog }
  { Журнал событий }
  TSysLog = class(TObject)
  private
    FItemsList: TLogItemList;
    FFilteredItemsList: TLogItemList;
    FWriteBuffer: ansistring;
    { Счетчик записей в буфере }
    WriteBufferCount: integer;
    { Счетчик тиков перед записью буфера }
    TicksCount: integer;
    procedure FlushBuffer();
  public
    { Имя файла по умолчанию }
    FileName: string;
    { Максимальное число записей для хранения в памяти }
    MaxRecords: integer;
    { Сколько записей набрать, прежде чем записать на диск, 0-без ограничений, -1 не писать в файл }
    WriteBufferDepth: integer;
    { Максимум тиков перед записью буфера, 0-при каждом тике, -1 не учитывать }
    TicksDepth: integer;
    Control: TComponent;
    constructor Create();
    destructor Destroy(); override;
    { Добавление записи в журнал }
    function AddRec(const ARec: TLogRecord): integer; overload;
    { Добавление сообщения в журнал с указанием полей }
    function AddRec(ASeverity: byte; AHostname, AAppname, AText: string): integer; overload;
    function Count(): integer;
    { Получение записи по номеру }
    function GetItem(Index: integer): ILogItem;
    function GetItemRec(Index: integer; var ALogRec: TLogRecord): Boolean;
    function GetItemStr(Index: integer): string;
    { Получение массива записей, совпадающих с фильтром }
    function GetFiltered(Filter: TLogRecord): TLogRecordArray;
    procedure Clear();
    { Чтение журнала из файла }
    function FromFile(AFileName: string): boolean;
    { Запись журнала в файл }
    function ToFile(AFileName: string): boolean;
    { Добавление заданной строки в файл }
    function AppendToFile(AFileName, AMsg: string): boolean;
    { Добавление записи из строки формата Syslog }
    procedure AddSyslogMsg(const Msg: string; IpAddr: string = '');
    { Тик записи буфера }
    procedure Tick();
  end;

  { TSysLogDaemon }
  { Сервер-приемник UDP-пакетов Syslog }
  TSysLogDaemon = class(TThread)
  private
    FOnMsg: TSyslogEvent;
    LastMessage: string;
    LastIP: string;
    Sock: TUDPBlockSocket;
    procedure SyncProc();
  protected
    procedure Execute(); override;
  public
    constructor Create();
    property OnMsg: TSyslogEvent read FOnMsg write FOnMsg;
  end;

implementation

function GetFirstWord(var s: string; separator: string = ' '): string;
var
  i: integer;
begin
  Result := '';
  i := Pos(separator, s);
  if i > 0 then
  begin
    Result := Copy(s, 1, i - 1);
    s := Copy(s, i + 1, maxint);
  end;
end;

function StrToLogRec(const Msg: string): TLogRecord;
var
  i, n: integer;
  year, mon, day, hour, min, sec: word;
  s, ss, s2: string;
begin
  ss := Msg;
  Result.Severity := 0;
  Result.Facility := 0;
  // PRIVAL and VERSION - '<PRIVAL>VERSION'
  s := GetFirstWord(ss);
  s2 := '';
  i := Pos('>', s);
  if i > 0 then
  begin
    n := Pos('<', s);
    s2 := Copy(s, i + 1, maxint); // VERSION
    s := Copy(s, n + 1, i - n - 1);   // PRIVAL
    Result.RFC5424 := (s2 = '1');
    // decode facil & loglev
    n := StrToIntDef(s, 0);
    Result.Facility := n div 8;
    Result.Severity := n mod 8;
  end;
  if Result.RFC5424 then  // syslog format (RFC 5424)
  begin
    // TIMESTAMP
    s := GetFirstWord(ss);
    Result.Timestamp := Now(); { TODO : do actual parsing }
    // HOSTNAME
    Result.Hostname := GetFirstWord(ss);
    // APPNAME
    Result.Appname := GetFirstWord(ss);
    // PROCID
    Result.ProcID := GetFirstWord(ss);
    // MSGID
    Result.MsgID := GetFirstWord(ss);
    // STRUCTURED-DATA
    s := GetFirstWord(ss);
    // MSG
    Result.Text := ss;
  end
  else // BSD format (RFC 3164)
  begin
    DecodeDate(Now(), year, mon, day);  // get current year
    // s2=(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
    //  1    6    11   16   21
    s := 'Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec';
    i := Pos(s2, s);
    mon := 1;
    if i > 0 then
      mon := ((i - 1) div 5) + 1;
    // DAY
    s := GetFirstWord(ss);
    day := StrToIntDef(s, 1);
    // TIME
    s := GetFirstWord(ss);
    hour := StrToIntDef(GetFirstWord(s, ':'), 0);
    min := StrToIntDef(GetFirstWord(s, ':'), 0);
    sec := StrToIntDef(s, 0);
    Result.Timestamp := EncodeDate(year, mon, day) + EncodeTime(hour, min, sec, 0);
    // HOSTNAME
    Result.Hostname := GetFirstWord(ss);
    // MSG-TAG
    s := GetFirstWord(ss);
    i := Pos(':', s);
    if i > 0 then
    begin
      Result.Appname := Copy(s, 1, i - 1);
      i := Pos('[', Result.Appname);
      if i > 0 then  // MSG-TAG-PID
      begin
        s := Result.Appname;
        n := Pos(']', s);
        Result.Appname := Copy(s, 1, i - 1);
        Result.ProcID := Copy(s, i + 1, n - i - 1);
      end;
      // MSG-CONTENT
      Result.Text := ss;
    end
    else
    begin
      // MSG-CONTENT
      Result.Text := s + ' ' + ss;
    end;
  end;
end;

function SeverityStr(const rec: TLogRecord): string;
begin
  case rec.Severity of
    0: Result := 'EMERG';
    1: Result := 'ALERT';
    2: Result := 'CRIT';
    3: Result := 'ERROR';
    4: Result := 'WARN';
    5: Result := 'NOTE';
    6: Result := 'INFO';
    7: Result := 'DEBUG';
    else
      Result := '';
  end;
end;


{ TLogItemList }

procedure TLogItemList.AddLogRec(const ALogRec: TLogRecord);
var
  TmpItem: ILogItem;
begin
  TmpItem := TLogItem.Create();
  TmpItem.SetLogRec(ALogRec);
  Add(TmpItem);
end;

function TLogItemList.GetLogRec(Index: integer; var ALogRec: TLogRecord
  ): Boolean;
var
  TmpItem: ILogItem;
begin
  TmpItem := ILogItem(Items[Index]);
  Result := TmpItem.GetLogRec(ALogRec);
end;

{ TLogItem }

procedure TLogItem.SetLogRec(const ALogRec: TLogRecord);
begin
  FLogRec := ALogRec;
end;

function TLogItem.GetLogRec(var ALogRec: TLogRecord): Boolean;
begin
  ALogRec := FLogRec;
  Result := True;
end;

function TLogItem.GetSeverity: Byte;
begin
  Result := FLogRec.Severity;
end;

function TLogItem.GetSeverityStr: string;
begin
  Result := SeverityStr(FLogRec);
end;

function TLogItem.ToStr: string;
begin
  Result := '';
  LogRecToSyslogStr(FLogRec, Result);
end;

{ TSysLogDaemon }

procedure TSysLogDaemon.SyncProc();
begin
  if Assigned(FOnMsg) then
    FOnMsg(self.LastMessage, self.LastIP);
end;

procedure TSysLogDaemon.Execute();
var
  buf: string;
begin
  Sock := TUDPBlockSocket.Create();
  try
    Sock.bind('0.0.0.0', '514');
    if Sock.LastError <> 0 then
    begin
      self.LastMessage := Sock.LastErrorDesc;
      //Synchronize(@SyncProc);
      Exit;
    end;
    while not Terminated do
    begin
      buf := Sock.RecvPacket(1000);
      self.LastIP := Sock.GetRemoteSinIP();
      if Sock.LastError = 0 then
      begin
        // do something with data and prepare response data
        //Sock.SendString(Buf);
        self.LastMessage := buf;
        Synchronize(@SyncProc);
      end;
      sleep(1);
    end;
    Sock.CloseSocket();
  finally
    Sock.Free();
  end;
end;

constructor TSysLogDaemon.Create();
begin
  inherited Create(False);
end;

{ TSysLog }

function TSysLog.AddRec(const ARec: TLogRecord): integer;
var
  TmpItem: ILogItem;
  s: AnsiString;
begin
  // удаление первой записи и сдвиг
  if FItemsList.Count >= MaxRecords then
  begin
    FItemsList.Delete(0);
  end;

  FItemsList.AddLogRec(ARec);
  Result := FItemsList.Count - 1;

  // add to file buffer
  if WriteBufferDepth >= 0 then
  begin
    s := '';
    if LogRecToSyslogStr(ARec, s) then
    begin
      FWriteBuffer := FWriteBuffer + s + CRLF;
      Dec(WriteBufferCount);
      if WriteBufferCount = 0 then
        FlushBuffer();
    end;
  end;
end;

function TSysLog.AddRec(ASeverity: byte; AHostname, AAppname, AText: string): integer;
var
  rec: TLogRecord;
begin
  rec.Timestamp := Now();
  rec.Severity := ASeverity;
  rec.Facility := 16;
  rec.Hostname := AHostname;
  rec.Appname := AAppname;
  rec.Text := AText;
  Result := self.AddRec(rec);
end;

function TSysLog.Count(): integer;
begin
  Result := FItemsList.Count;
end;

function TSysLog.GetItem(Index: integer): ILogItem;
begin
  if (Index >= 0) and (Index < FItemsList.Count) then
    Result := ILogItem(FItemsList[Index])
  else
    Result := nil;
end;

function TSysLog.GetItemRec(Index: integer; var ALogRec: TLogRecord): Boolean;
begin
  Result := False;
  if (Index >= 0) and (Index < FItemsList.Count) then
  begin
    Result := FItemsList.GetLogRec(Index, ALogRec);
  end;
end;

function TSysLog.GetItemStr(Index: integer): string;
var
  TmpLogRec: TLogRecord;
begin
  Result := '';
  if GetItemRec(Index, TmpLogRec) then
    LogRecToSyslogStr(TmpLogRec, Result);
end;

function TSysLog.GetFiltered(Filter: TLogRecord): TLogRecordArray;
var
  i, res_count: integer;
  rec: TLogRecord;
  doAdd: boolean;
begin
  res_count := 0;
  SetLength(Result, res_count);
  for i := 0 to Count() - 1 do
  begin
    GetItemRec(i, rec);
    doAdd := False;
    if (not doAdd) and (Filter.Severity > 0) and (rec.Severity = Filter.Severity) then
    begin
      doAdd := True;
    end;

    if (not doAdd) and (Filter.Hostname <> '') and (rec.Hostname = Filter.Hostname) then
    begin
      doAdd := True;
    end;

    if (not doAdd) and (Filter.Appname <> '') and (rec.Appname = Filter.Appname) then
    begin
      doAdd := True;
    end;

    if doAdd then
    begin
      Inc(res_count);
      SetLength(Result, res_count);
      Result[res_count - 1] := rec;
    end;
  end;
end;

procedure TSysLog.Clear();
{var
  i: Integer; }
begin
  {for i := FItemsList.Count-1 downto 0 do
    TLogItem(FItemsList[i]).Free(); }
  FFilteredItemsList.Clear();
  FItemsList.Clear();
end;

function TSysLog.FromFile(AFileName: string): boolean;
var
  i: integer;
  s: string;
  sl: TStringList;
  rec: TLogRecord;
begin
  self.Clear();

  sl := TStringList.Create();
  try
    sl.LoadFromFile(AFileName);
    for i := 0 to sl.Count - 1 do
    begin
      s := sl[i];
      rec := StrToLogRec(s);
      self.AddRec(rec);
    end;
  finally
    sl.Free();
  end;
  Result := True;
end;

function TSysLog.ToFile(AFileName: string): boolean;
var
  i: integer;
  s: string;
  sl: TStringList;
begin
  sl := TStringList.Create();
  try
    for i := 0 to Count() - 1 do
    begin
      sl.Add(GetItem(i).ToStr());
    end;
    sl.SaveToFile(AFileName);
  finally
    sl.Free();
  end;
  Result := True;
end;

function TSysLog.AppendToFile(AFileName, AMsg: string): boolean;
var
  f: TextFile;
begin
  AssignFile(f, AFileName);
  if FileExists(AFileName) then
    Append(f)
  else
    Rewrite(f);
  Writeln(f, AMsg);
  Flush(f);
  CloseFile(f);
  Result := True;
end;

procedure TSysLog.AddSyslogMsg(const Msg: string; IpAddr: string);
var
  rec: TLogRecord;
begin
  rec := StrToLogRec(Msg);
  rec.IpAddress := IpAddr;
  self.AddRec(rec);
end;

procedure TSysLog.Tick();
begin
  Dec(TicksCount);
  if TicksCount = 0 then
    FlushBuffer();
end;

procedure TSysLog.FlushBuffer();
begin
  WriteBufferCount := WriteBufferDepth;
  TicksCount := TicksDepth;
  if Length(FWriteBuffer) > 0 then
    self.AppendToFile(self.FileName, FWriteBuffer);
  FWriteBuffer := '';
end;

constructor TSysLog.Create();
begin
  //inherited Create();
  FItemsList := TLogItemList.Create();
  FFilteredItemsList := TLogItemList.Create();

  self.MaxRecords := 100;
  self.WriteBufferDepth := 10;
  self.TicksDepth := 1;
  self.Clear();
end;

destructor TSysLog.Destroy();
begin
  self.Clear();
  FreeAndNil(FFilteredItemsList);
  FreeAndNil(FItemsList);
  inherited Destroy;
end;

end.
