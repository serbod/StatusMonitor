unit log_service;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, blcksock, Logger, SimSyn;

type
  TSyslogEvent = procedure(const Msg: string; const IPAddr: string) of object;

function StrToLogRec(const Msg: string): TLogRecord;
function SeverityStr(const rec: TLogRecord): string;

type

  { TLogItem }

  TLogItem = class
  public
    LogLevel: TLogLevel;
    Timestamp: TDateTime;
    HostnameID: Integer;
    AppnameID: Integer;
    IpAddress: LongWord;
    Text: string;
    procedure BeforeDestruction; override;
  end;

  { TLogItemList }

  TLogItemList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    ParentList: TLogItemList;
    MaxItems: Integer;
    constructor Create(AMaxItems: Integer; AParentList: TLogItemList = nil);
    function GetItem(AIndex: Integer): TLogItem;
  end;


  TLogRecordArray = array of TLogRecord;

  { TSysLog }
  { Журнал событий }
  TSysLog = class(TObject)
  private
    FItemsList: TLogItemList;
    FFreezeList: TLogItemList;
    FFilteredList: TLogItemList;
    FWriteBuffer: AnsiString;
    FHostnameList: TStringList;
    FAppnameList: TStringList;

    FMaxCount: Integer;
    FFilterEnabled: Boolean;
    FFreezeEnabled: Boolean;
    FFilterStr: string;
    { Счетчик записей в буфере }
    WriteBufferCount: integer;
    { Счетчик тиков перед записью буфера }
    TicksCount: integer;
    procedure FlushBuffer();
    function GetHostnameIndex(AHostname: string): Integer;
    function GetAppnameIndex(AAppname: string): Integer;
    procedure SetFilterStr(AValue: string);
    procedure SetFreeze(AValue: Boolean);
    function CheckFilter(AItem: TLogItem): Boolean;
    procedure UpdateFilteredList();
  public
    RWLock: TSimpleRWLock;
    { Имя файла по умолчанию }
    FileName: string;
    { Сколько записей набрать, прежде чем записать на диск, 0-без ограничений, -1 не писать в файл }
    WriteBufferDepth: integer;
    { Максимум тиков перед записью буфера, 0-при каждом тике, -1 не учитывать }
    TicksDepth: integer;
    { For export to file }
    TimestampFormat: string;
    Control: TComponent;
    constructor Create();
    destructor Destroy(); override;
    { Добавление записи в журнал }
    procedure AddRec(const ARec: TLogRecord); overload;
    { Добавление сообщения в журнал с указанием полей }
    procedure AddRec(ASeverity: byte; AHostname, AAppname, AText: string); overload;
    function Count(): integer;
    { Получение записи по номеру }
    function GetItem(Index: integer): TLogItem;
    function GetItemLoglevel(Index: integer): TLogLevel;
    function GetItemRec(Index: integer; var ALogRec: TLogRecord): Boolean;
    function GetItemStr(Index: integer): string;
    { Получение Hostname }
    function GetHostnameByIndex(AIndex: Integer): string;
    { Получение Appname }
    function GetAppnameByIndex(AIndex: Integer): string;
    { Получение массива записей, совпадающих с фильтром }
    //function GetFiltered(Filter: TLogRecord): TLogRecordArray;
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
    { Максимальное число элементов }
    property MaxCount: Integer read FMaxCount write FMaxCount;
    { Фильтр по строке }
    property FilterStr: string read FFilterStr write SetFilterStr;
    { Заморозка, отображается сохраненный список событий }
    property Freeze: Boolean read FFreezeEnabled write SetFreeze;
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

  function LongWordToIPStr(IP: LongWord): string;

implementation

// извлекает первое слово из строки, по умолчанию разделитель пробел
function ExtractFirstWord(var s: string; separator: string = ' '): string;
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
  s := ExtractFirstWord(ss);
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
    s := ExtractFirstWord(ss);
    Result.Timestamp := Now(); { TODO : do actual parsing }
    // HOSTNAME
    Result.Hostname := ExtractFirstWord(ss);
    // APPNAME
    Result.Appname := ExtractFirstWord(ss);
    // PROCID
    Result.ProcID := ExtractFirstWord(ss);
    // MSGID
    Result.MsgID := ExtractFirstWord(ss);
    // STRUCTURED-DATA
    s := ExtractFirstWord(ss);
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
    s := ExtractFirstWord(ss);
    day := StrToIntDef(s, 1);
    // TIME
    s := ExtractFirstWord(ss);
    hour := StrToIntDef(ExtractFirstWord(s, ':'), 0);
    min := StrToIntDef(ExtractFirstWord(s, ':'), 0);
    sec := StrToIntDef(s, 0);
    Result.Timestamp := EncodeDate(year, mon, day) + EncodeTime(hour, min, sec, 0);
    // HOSTNAME
    Result.Hostname := ExtractFirstWord(ss);
    // MSG-TAG
    s := ExtractFirstWord(ss);
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

function IPStrToLongWord(const AIpStr: string): LongWord;
var
  ss: string;
  n1, n2, n3, n4: Integer;
begin
  Result := 0;
  ss := AIpStr;

  n1 := StrToIntDef(ExtractFirstWord(ss, '.'), -1);
  if (n1 < 0) or (n1 > 255) then Exit;
  n2 := StrToIntDef(ExtractFirstWord(ss, '.'), -1);
  if (n2 < 0) or (n2 > 255) then Exit;
  n3 := StrToIntDef(ExtractFirstWord(ss, '.'), -1);
  if (n3 < 0) or (n3 > 255) then Exit;
  n4 := StrToIntDef(ExtractFirstWord(ss, '.'), -1);
  if (n4 < 0) or (n4 > 255) then Exit;

  Result := n1 or (n2 shl 8) or (n3 shl 16) or (Byte(n4) shl 24);
end;

function LongWordToIPStr(IP: LongWord): string;
begin
  Result := IntToStr((IP and $000000FF) shr 00)
    + '.' + IntToStr((IP and $0000FF00) shr 08)
    + '.' + IntToStr((IP and $00FF0000) shr 16)
    + '.' + IntToStr((IP and $FF000000) shr 24);
end;


{ TLogItemList }

procedure TLogItemList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) and (not Assigned(ParentList)) then
    TLogItem(Ptr).Free();
end;

constructor TLogItemList.Create(AMaxItems: Integer; AParentList: TLogItemList);
begin
  inherited Create();
  MaxItems := AMaxItems;
  ParentList := AParentList;
end;

function TLogItemList.GetItem(AIndex: Integer): TLogItem;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := TLogItem(Get(AIndex))
  else
    Result := nil;
end;

{ TLogItem }

procedure TLogItem.BeforeDestruction;
begin
  inherited BeforeDestruction;
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

procedure TSysLog.AddRec(const ARec: TLogRecord);
var
  s: AnsiString;
  TmpItem: TLogItem;
begin
  TmpItem := TLogItem.Create();
  TmpItem.HostnameID := GetHostnameIndex(ARec.Hostname);
  TmpItem.AppnameID := GetAppnameIndex(ARec.Appname);
  TmpItem.IpAddress := IPStrToLongWord(ARec.IpAddress);
  TmpItem.LogLevel := TLogLevel(ARec.Severity);
  TmpItem.Timestamp := ARec.Timestamp;
  TmpItem.Text := ARec.Text;

  if RWLock.BeginWrite() then
  begin
    FItemsList.Add(TmpItem);
    if FFilterEnabled and (not FFreezeEnabled) and CheckFilter(TmpItem) then
      FFilteredList.Add(TmpItem);
    // очистка избыточных записей
    if FItemsList.Count > (MaxCount - (MaxCount div 10)) then
    begin
      while FFilteredList.Count > MaxCount do
        FFilteredList.Delete(0);

      while FItemsList.Count > MaxCount do
      begin
        FFilteredList.Remove(FItemsList[0]);
        FItemsList.Delete(0);
      end;
    end;
    RWLock.EndWrite();
  end;

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

procedure TSysLog.AddRec(ASeverity: byte; AHostname, AAppname, AText: string);
var
  rec: TLogRecord;
begin
  rec.Timestamp := Now();
  rec.Severity := ASeverity;
  rec.Facility := 16;
  rec.Hostname := AHostname;
  rec.Appname := AAppname;
  rec.Text := AText;
  self.AddRec(rec);
end;

function TSysLog.Count(): integer;
begin
  if FFilterEnabled then
    Result := FFilteredList.Count
  else if not FFreezeEnabled then
    Result := FItemsList.Count
  else
    Result := FFreezeList.Count;
end;

function TSysLog.GetItem(Index: integer): TLogItem;
begin
  if FFilterEnabled then
  begin
    if (Index >= 0) and (Index < FFilteredList.Count) then
      Result := TLogItem(FFilteredList[Index])
    else
      Result := nil;
  end
  else if not FFreezeEnabled then
  begin
    if (Index >= 0) and (Index < FItemsList.Count) then
      Result := TLogItem(FItemsList[Index])
    else
      Result := nil;
  end
  else
  begin
    if (Index >= 0) and (Index < FFreezeList.Count) then
      Result := TLogItem(FFreezeList[Index])
    else
      Result := nil;
  end;
end;

function TSysLog.GetItemLoglevel(Index: integer): TLogLevel;
var
  TmpItem: TLogItem;
begin
  Result := llAlert;
  if RWLock.BeginRead() then
  begin
    TmpItem := GetItem(Index);
    if Assigned(TmpItem) then
      Result := TmpItem.LogLevel;

    RWLock.EndRead();
  end;
end;

function TSysLog.GetItemRec(Index: integer; var ALogRec: TLogRecord): Boolean;
var
  TmpItem: TLogItem;
begin
  Result := False;
  if RWLock.BeginRead() then
  begin
    TmpItem := GetItem(Index);
    if Assigned(TmpItem) then
    begin
      ALogRec.Timestamp := TmpItem.Timestamp;
      ALogRec.Text := TmpItem.Text;
      ALogRec.Severity := Ord(TmpItem.LogLevel);
      ALogRec.IpAddress := LongWordToIPStr(TmpItem.IpAddress);
      ALogRec.Appname := GetAppnameByIndex(TmpItem.AppnameID);
      ALogRec.Hostname := GetHostnameByIndex(TmpItem.HostnameID);
      Result := True;
    end;
    RWLock.EndRead();
  end;
end;

function TSysLog.GetItemStr(Index: integer): string;
var
  //TmpLogRec: TLogRecord;
  TmpItem: TLogItem;
begin
  Result := '';
  if RWLock.BeginRead() then
  begin
    TmpItem := GetItem(Index);
    if Assigned(TmpItem) then
    begin
      Result := FormatDateTime(TimestampFormat, TmpItem.Timestamp)
             + ' ' + LLToStr(TmpItem.LogLevel)
             + ' ' + TmpItem.Text + CRLF;
    end;

    RWLock.EndRead();
  end;
  {if GetItemRec(Index, TmpLogRec) then
    LogRecToSyslogStr(TmpLogRec, Result); }
end;

{
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
}

procedure TSysLog.Clear();
begin
  if RWLock.BeginWrite() then
  begin
    FFilteredList.Clear();
    FFreezeList.Clear();
    FItemsList.Clear();
    FAppnameList.Clear();
    FHostnameList.Clear();
    RWLock.EndWrite();
  end;
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
  rec: TLogRecord;
begin
  sl := TStringList.Create();
  try
    for i := 0 to Count() - 1 do
    begin
      GetItemRec(i, rec);
      rec.RFC5424 := True;
      s := '';
      if LogRecToSyslogStr(rec, s) then
        sl.Add(s);
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

function TSysLog.GetHostnameIndex(AHostname: string): Integer;
begin
  Result := FHostnameList.IndexOf(AHostname);
  if Result = -1 then
    Result := FHostnameList.Add(AHostname);
end;

function TSysLog.GetHostnameByIndex(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < FHostnameList.Count) then
    Result := FHostnameList[AIndex]
  else
    Result := '';
end;

function TSysLog.GetAppnameIndex(AAppname: string): Integer;
begin
  Result := FAppnameList.IndexOf(AAppname);
  if Result = -1 then
    Result := FAppnameList.Add(AAppname);
end;

procedure TSysLog.SetFilterStr(AValue: string);
begin
  if FFilterStr = AValue then Exit;
  FFilterStr := AnsiLowerCase(Trim(AValue));
  FFilterEnabled := (Length(FFilterStr) > 0);
  UpdateFilteredList();
end;

procedure TSysLog.SetFreeze(AValue: Boolean);
var
  i: Integer;
  TmpItem, NewItem: TLogItem;
begin
  if FFreezeEnabled = AValue then Exit;
  FFreezeEnabled := AValue;
  if RWLock.BeginRead() then
  begin
    FFreezeList.Clear();
    if FFreezeEnabled then
    begin
      // копирование элементов в список заморозки
      for i := 0 to FItemsList.Count-1 do
      begin
        TmpItem := TLogItem(FItemsList.Items[i]);
        NewItem := TLogItem.Create();
        NewItem.Text := TmpItem.Text;
        NewItem.LogLevel := TmpItem.LogLevel;
        NewItem.Timestamp := TmpItem.Timestamp;
        NewItem.AppnameID := TmpItem.AppnameID;
        NewItem.HostnameID := TmpItem.HostnameID;
        NewItem.IpAddress := TmpItem.IpAddress;
        FFreezeList.Add(NewItem);
      end;
    end;
    RWLock.EndRead();
  end;
end;

function TSysLog.GetAppnameByIndex(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < FAppnameList.Count) then
    Result := FAppnameList[AIndex]
  else
    Result := '';
end;

function TSysLog.CheckFilter(AItem: TLogItem): Boolean;
begin
  Result := True;
  if FFilterEnabled and Assigned(AItem) then
  begin
    if (FFilterStr <> '')
    and (Pos(FFilterStr, AnsiLowerCase(AItem.Text)) = 0)
    and (Pos(FFilterStr, FormatDateTime('hh:nn:ss.zzz', AItem.Timestamp)) = 0)
    then
      Result := False;
  end;
end;

procedure TSysLog.UpdateFilteredList();
var
  i: Integer;
  Item: TLogItem;
begin
  if RWLock.BeginWrite() then
  try
    FFilteredList.Clear();
    if FFilterEnabled then
    begin
      if FFreezeEnabled then
      begin
        for i := 0 to FFreezeList.Count-1 do
        begin
          Item := TLogItem(FFreezeList.Items[i]);
          if CheckFilter(Item) then
            FFilteredList.Add(Item);
        end;
      end
      else
      begin
        for i := 0 to FItemsList.Count-1 do
        begin
          Item := TLogItem(FItemsList.Items[i]);
          if CheckFilter(Item) then
            FFilteredList.Add(Item);
        end;
      end;
    end
  finally
    RWLock.EndWrite();
  end;
end;

constructor TSysLog.Create();
begin
  //inherited Create();
  FItemsList := TLogItemList.Create(10000);
  FFreezeList := TLogItemList.Create(10000);
  FFilteredList := TLogItemList.Create(10000, FItemsList);
  FHostnameList := TStringList.Create();
  FAppnameList := TStringList.Create();
  RWLock.Init();

  FMaxCount := 10000;
  self.WriteBufferDepth := 10;
  self.TicksDepth := 1;
  //TimestampFormat := 'DD-MM-YYYY HH:NN:SS.ZZZ';
  TimestampFormat := 'HH:NN:SS.ZZZ';
  self.Clear();

end;

destructor TSysLog.Destroy();
begin
  self.Clear();
  FreeAndNil(FAppnameList);
  FreeAndNil(FHostnameList);
  FreeAndNil(FFilteredList);
  FreeAndNil(FFreezeList);
  FreeAndNil(FItemsList);
  inherited Destroy;
end;

end.
