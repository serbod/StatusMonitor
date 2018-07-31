unit services;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, pingsend, log_service, blcksock, process, Logger,
  LConvEncoding, comobj, Forms, IniFiles;

type

  { TVoiceAlert }

  TVoiceAlert = class(TThread)
  private
    FText: TStringList;
    FSavedCW: word;
    FActive: boolean;
    FSpVoice: variant;
  protected
    procedure Execute; override;
  public
    constructor Create(); reintroduce;
    destructor Destroy; override;
    procedure Speak(const s: string);
    procedure AddText(const s: string);
  end;

  { TChecker }
  { Чекер, базовый класс проверки чего-либо в отдельном потоке.
  Подключается к сервису. При обновлении состояния проверяемого объекта генерирует
  событие OnChecker, в котором можно глянуть ResultCode и Comment проверки }
  TChecker = class(TThread)
  private
    FOnChecker: TNotifyEvent;
    procedure SyncProc();
  public
    { Код результата проверки 0-норма, 1-критично, 2-некритично }
    ResultCode: integer;
    { Текстовый результат проверки }
    Comment: string;
    { Состояние активности }
    Active: boolean;
    procedure Start();
    procedure Stop();
    property OnChecker: TNotifyEvent read FOnChecker write FOnChecker;
  end;

  { TMonService }
  { Сервис мониторинга, отражает состояние какого-либо устройства
  или программы, содержит чекер и журнал событий }
  TMonService = class(TObject)
  private
    FChecker: TChecker;
    FLog: TSysLog;
    FOptions: TStringList;
    FVoiceAlert: TVoiceAlert;
    FAlertMuteKeywords: TStringList;
    procedure OnCheckerHandler(Sender: TObject);
    procedure SetChecker(Value: TChecker);
    function CheckAlert(const AText: string): boolean;
    procedure SetVoiceAlert(AValue: TVoiceAlert);
  public
    { Внутренее название сервиса (латиница) }
    Name: string;
    { Название группы, отображается в GUI }
    Group: string;
    { Внешнее название, отображается в GUI }
    FullName: string;
    { Название хоста, сообщения от которого перехватываются этим сервисом }
    IpAddr: string;
    HostName: string;
    { Название приложения, сообщения от которого перехватываются этим сервисом }
    AppName: string;
    { Код состояния 0-норма, 1-критично, 2-некритично }
    State: integer;
    { Описание сервиса }
    Comment: string;
    { Адрес вышестоящего сервера }
    SyslogServer: string;
    { Счетчик ошибок }
    ErrorCounter: integer;
    { Время последнего события }
    LastEventTime: TDateTime;
    VoiceStrWarning: string;
    VoiceStrCritical: string;
    { Кодировка входящих сообщений }
    Encoding: string;

    constructor Create(AName: string);
    destructor Destroy(); override;

    { Чекер }
    property Checker: TChecker read FChecker write SetChecker;
    { Жкрнал событий (лог) }
    property Log: TSysLog read FLog;
    { Настройки, пары имя=значение }
    property Options: TStringList read FOptions;
    { Голосовой оповещатель }
    property VoiceAlert: TVoiceAlert read FVoiceAlert write SetVoiceAlert;
    { Слова отключающие тревогу }
    property AlertMuteKeywords: TStringList read FAlertMuteKeywords;

    procedure AddLogMessage(ASeverity: byte; const AHostname, AAppname, AText: string);
    procedure SaveToConfig(AConf: TIniFile; ASection: string);
    procedure LoadFromConfig(AConf: TIniFile; ASection: string);
  end;

  { TMonServiceList - список сервисов }

  TMonServiceList = class(TObjectList)
  public
    { Возвращает сервис по внутреннему имени }
    function GetByName(const AName: string): TMonService;
    { Возвращает сервис по порядковому номеру }
    function GetByIndex(Index: integer): TMonService;
    { Создает новый сервис с заданным именем }
    function AddService(const AName, AFullName: string): TMonService;
    { Создает новый сервис с заданным именем и чекером }
    function AddService(const AName, AFullName: string; AChecker: TChecker): TMonService;
    { сортировать с учетом группы }
    procedure SortByGroup();
    { сортировать с учетом времени простоя }
    procedure SortBySleepTime();
  end;

  { TPingChecker }
  { Чекер, пингующий заданный адрес
  Comment - значение пинга в мс.
  ResultCode - 0-пинг до 1000 мс, 1-нет ответа, 2-пинг более 1000 мс
  }
  TPingChecker = class(TChecker)
  private
    { Адрес для пингования }
    IPAddr: string;
    FWarnings: integer;
    FErrors: integer;
    FOKs: integer;
  protected
    procedure Execute(); override;
  public
    WarningValue: integer;
    WarningCount: integer;
    WarningResetCount: integer;
    ErrorValue: integer;
    ErrorCount: integer;
    ErrorResetCount: integer;
    PingInterval: integer;
    PingIntervalValue: integer;
    constructor Create(const AIPAddr: string); reintroduce;
  end;

  { TConsoleChecker }
  { Чекер консольного приложения
  Comment - последнее сообщение STDOUT.
  ResultCode - 0-работает, 1-не работает, 2-есть ошибки
  }
  TConsoleChecker = class(TChecker)
  private
    FWarnings: integer;
    FErrors: integer;
    FOKs: integer;
    AProcess: TProcess;
  protected
    procedure Execute(); override;
  public
    CommandStr: string;
    WarningValue: string;
    WarningCount: integer;
    WarningResetCount: integer;
    ErrorValue: string;
    ErrorCount: integer;
    ErrorResetCount: integer;
    constructor Create(const ACommandStr: string); reintroduce;
    function ReadConsole(): string;
  end;

  { TSysLogServer }
  { Сервер, принимающий сообщения по протоколу SysLog
  Сообщения разделяются по разным сервисам, где внутреннее имя сервиса соответсвует
  полю AppName сообщения. Прочие сообщения попадают в DefaultLog }
  TSysLogServer = class(TObject)
  private
    FAppNames: TStringList;
    FAlertMuteKeywords: TStringList;
    FSysLogDaemon: TSysLogDaemon;
    FMonServiceList: TMonServiceList;
    FDefaultLog: TSysLog;
    FVoiceAlert: TVoiceAlert;
    procedure OnMsgHandler(const Msg: string; const IPAddr: string);
    function SendLogRec(const rec: TLogRecord; host: string): Boolean;
    procedure SetVoiceAlert(AValue: TVoiceAlert);
  public
    UplinkServer: string;
    UplinkServer2: string;
    AutoCreateLoggers: boolean;
    DefaultEncoding: string;
    VoiceStrWarning: string;
    VoiceStrCritical: string;
    VoiceStrNewService: string;
    constructor Create(AMonServiceList: TMonServiceList);
    destructor Destroy(); override;

    property AppNames: TStringList read FAppNames;
    property AlertMuteKeywords: TStringList read FAlertMuteKeywords;
    property SysLogDaemon: TSysLogDaemon read FSysLogDaemon;
    property MonServiceList: TMonServiceList read FMonServiceList;
    property DefaultLog: TSysLog read FDefaultLog;
    property VoiceAlert: TVoiceAlert read FVoiceAlert write SetVoiceAlert;
  end;

implementation

function GetIpAddrList(): string;
var
  AProcess: TProcess;
  s: string;
  sl: TStringList;
  i: integer;

begin
  Result := '';
  sl := TStringList.Create();
  {$IFDEF WINDOWS}
  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := 'ipconfig.exe';
  AProcess.Options := AProcess.Options + [poUsePipes, poNoConsole];
  try
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
    if Pos('IPv4', sl[i]) = 0 then
      Continue;
    s := sl[i];
    Result := Result + Trim(Copy(s, Pos(':', s) + 1, 999)) + '  ';
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
    //Sleep(500); // poWaitOnExit not works as expected
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
  sl.Free();
end;

{ TVoiceAlert }

procedure TVoiceAlert.Speak(const s: string);
var
  ss: variant;
begin
  {$IFDEF WINDOWS}
  if not FActive then
  begin
    FSpVoice := CreateOleObject('SAPI.SpVoice');
    FSpVoice.Volume := 100;
    FActive := True;
  end
  else
  begin
    //FSpVoice := GetActiveOleObject('SAPI.SpVoice');
  end;
  // Change FPU interrupt mask to avoid SIGFPE exceptions
  //FSavedCW := Get8087CW;
  try
    //Set8087CW(FSavedCW or $4);
    ss := s;
    FSpVoice.Speak(ss, 1);
    //FSpVoice.DisplayUI(Application.MainFormHandle, 'title', 'SpeechAddRemoveWord');
    //FSpVoice.WaitUntilDone(5000);
  finally
    // Restore FPU mask
    //Set8087CW(FSavedCW);
  end;
  //FSpVoice:=nil;
  {$ENDIF}
end;

procedure TVoiceAlert.Execute();
var
  ss: variant;
begin
  while not Terminated do
  begin
    if FText.Count > 0 then
    begin
      ss := FText[0];
      FText.Delete(0);
      FSpVoice := CreateOleObject('SAPI.SpVoice');
      FSpVoice.Volume := 100;
      // Change FPU interrupt mask to avoid SIGFPE exceptions
      FSavedCW := Get8087CW;
      try
        Set8087CW(FSavedCW or $4);
        FSpVoice.Speak(ss, 0);
        //FSpVoice.DisplayUI(Application.MainFormHandle, 'title', 'SpeechAddRemoveWord');
        //FSpVoice.WaitUntilDone(5000);
      finally
        // Restore FPU mask
        Set8087CW(FSavedCW);
        FSpVoice := nil;
      end;
    end;
    sleep(10);
  end;
end;

constructor TVoiceAlert.Create();
begin
  inherited Create(True);
  FText := TStringList.Create();
  FSpVoice := nil;
  FActive := False;
  self.Start();
end;

destructor TVoiceAlert.Destroy();
begin
  FreeAndNil(FText);
  inherited Destroy;
end;

procedure TVoiceAlert.AddText(const s: string);
begin
  FText.Add(s);
end;


{ TConsoleChecker }

procedure TConsoleChecker.Execute();
var
  sl: TStringList;
  i: integer;
begin
  //self.FreeOnTerminate := True;
  self.Active := True;

  sl := TStringList.Create();
  AProcess := TProcess.Create(nil);
  try
    AProcess.CommandLine := self.CommandStr;
    //AProcess.Executable := self.CommandStr;
    AProcess.Options := AProcess.Options + [poUsePipes, poNoConsole, poStderrToOutPut];
    AProcess.Execute();
    while not Terminated do
    begin
      Sleep(500);
      sl.Clear();
      sl.LoadFromStream(AProcess.Output);
      for i := 0 to sl.Count - 1 do
      begin
        //self.Comment:=CP1251ToUTF8(sl[i]);
        self.Comment := CP866ToUTF8(sl[i]);
        Synchronize(@SyncProc);
      end;
    end;
  finally
    AProcess.Free();
    sl.Free();
  end;

  self.Terminate;

end;

constructor TConsoleChecker.Create(const ACommandStr: string);
begin
  inherited Create(True);
  self.CommandStr := ACommandStr;
end;

function TConsoleChecker.ReadConsole(): string;
var
  sl: TStringList;
begin
  Result := '';
  if not Assigned(AProcess) then
    Exit;
  sl := TStringList.Create();
  sl.LoadFromStream(AProcess.Output);
  Result := CP866ToUTF8(sl.Text);
  sl.Free();
end;

{ TSysLogServer }

procedure TSysLogServer.OnMsgHandler(const Msg: string; const IPAddr: string);
var
  rec, rec2: TLogRecord;
  i, n: integer;
  monser: TMonService;
  s, ss: string;
begin
  rec := StrToLogRec(Msg);
  rec.IpAddress := IPAddr;

  if self.UplinkServer <> '' then
  begin
    rec2 := rec;
    rec2.Hostname := rec2.IpAddress + '_' + rec2.Hostname;
    SendLogRec(rec2, self.UplinkServer);

    if self.UplinkServer2 <> '' then
    begin
      SendLogRec(rec2, self.UplinkServer2);
    end;
  end;

  if (Length(rec.Appname) <= 1) and (Length(rec.Hostname) <= 1) then
  begin
    self.DefaultLog.AddRec(rec);
    Exit;
  end;
  // find app for record
  for i := 0 to self.MonServiceList.Count - 1 do
  begin
    monser := self.MonServiceList.GetByIndex(i);
    // фильтруем сервисы по именам
    // если указано только одно имя, то второе любое
    // если указано оба имени, то должны совпасть оба
    // если имена не указаны, то нафиг
    n := 0;
    if Length(monser.IpAddr) = 0 then
      Inc(n);
    if Length(monser.HostName) = 0 then
      Inc(n);
    if Length(monser.AppName) = 0 then
      Inc(n);
    if n = 3 then
      Continue;
    if (Pos(Upcase(monser.IpAddr), Upcase(rec.IpAddress)) > 0) then
      Inc(n);
    if (Pos(Upcase(monser.HostName), Upcase(rec.Hostname)) > 0) then
      Inc(n);
    if (Pos(Upcase(monser.AppName), Upcase(rec.Appname)) > 0) then
      Inc(n);

    if n = 3 then
    begin
      monser.AddLogMessage(rec.Severity, IPAddr + ' ' + rec.Hostname, rec.Appname, rec.Text);
      Exit;
    end;
  end;

  if AutoCreateLoggers then
  begin
    s := Trim(rec.Appname);
    ss := self.AppNames.Values[s];
    if ss = '' then
      ss := Trim(rec.Appname);
    ss := ss + ': ' + rec.IpAddress + ' ' + Trim(rec.Hostname);

    monser := self.MonServiceList.AddService(s + '_' + Trim(rec.Hostname), ss);
    monser.IpAddr := rec.IpAddress;
    monser.AppName := rec.Appname;
    monser.HostName := rec.Hostname;
    monser.VoiceAlert := self.VoiceAlert;
    monser.AlertMuteKeywords.Assign(self.AlertMuteKeywords);
    monser.VoiceStrWarning := self.VoiceStrWarning;
    monser.VoiceStrCritical := self.VoiceStrCritical;
    monser.AddLogMessage(rec.Severity, IPAddr + ' ' + rec.Hostname, rec.Appname, rec.Text);
    monser.Encoding := self.DefaultEncoding;
    // voice alert
    if Assigned(VoiceAlert) and (VoiceStrNewService <> '') then
    begin
      VoiceAlert.Speak(VoiceStrNewService);
    end;
    Exit;
  end;
  // не один сервис не подошел - в default
  self.DefaultLog.AddRec(rec);
end;

function TSysLogServer.SendLogRec(const rec: TLogRecord; host: string): Boolean;
var
  FSock: TUDPBlockSocket;
  s: AnsiString;
begin
  s := '';
  if LogRecToSyslogStr(rec, s) then
  begin
    FSock := TUDPBlockSocket.Create();
    try
      if host = '' then
        host := 'localhost';
      FSock.Connect(host, '514');
      FSock.SendString(s);
    finally
      FreeAndNil(FSock);
    end;
  end;
end;

procedure TSysLogServer.SetVoiceAlert(AValue: TVoiceAlert);
begin
  if FVoiceAlert = AValue then Exit;
  FVoiceAlert := AValue;
end;

constructor TSysLogServer.Create(AMonServiceList: TMonServiceList);
begin
  self.UplinkServer := '';
  self.FAppNames := TStringList.Create();
  self.FAlertMuteKeywords := TStringList.Create();
  self.FMonServiceList := AMonServiceList;
  self.FDefaultLog := TSysLog.Create();
  self.FDefaultLog.FileName := 'log/default.log';
  self.FSysLogDaemon := TSysLogDaemon.Create();
  self.FSysLogDaemon.OnMsg := @OnMsgHandler;
end;

destructor TSysLogServer.Destroy();
begin
  FreeAndNil(FSysLogDaemon);
  FreeAndNil(FDefaultLog);
  FreeAndNil(FAlertMuteKeywords);
  FreeAndNil(FAppNames);
  inherited Destroy;
end;

{ TChecker }

procedure TChecker.SyncProc();
begin
  if Assigned(FOnChecker) then
    FOnChecker(self);
end;

procedure TChecker.Start();
begin
  Suspended := False;
end;

procedure TChecker.Stop();
begin
  Suspended := True;
end;

{ TMonServiceList }

function TMonServiceList.GetByName(const AName: string): TMonService;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to self.Count - 1 do
  begin
    if (self.Items[i] as TMonService).Name = AName then
    begin
      Result := (self.Items[i] as TMonService);
      Exit;
    end;
  end;
end;

function TMonServiceList.GetByIndex(Index: integer): TMonService;
begin
  Result := nil;
  if (Index >= 0) and (Index < self.Count) then
  begin
    Result := (self.Items[Index] as TMonService);
  end;
end;

function TMonServiceList.AddService(const AName, AFullName: string): TMonService;
begin
  Result := TMonService.Create(AName);
  Result.FullName := AFullName;
  Result.State := 0;
  self.Add(Result);
end;

function TMonServiceList.AddService(const AName, AFullName: string;
  AChecker: TChecker): TMonService;
begin
  Result := TMonService.Create(AName);
  Result.FullName := AFullName;
  Result.State := 0;
  Result.Checker := AChecker;
  self.Add(Result);
end;

function CompareMonServices(Item1, Item2: Pointer): integer;
var
  s1, s2: string;
begin
  s1 := TMonService(Item1).Group;
  s2 := TMonService(Item2).Group;
  Result := CompareText(s1, s2);
  if Result = 0 then
  begin
    s1 := TMonService(Item1).FullName;
    s2 := TMonService(Item2).FullName;
    Result := CompareText(s1, s2);
  end;
end;

procedure TMonServiceList.SortByGroup();
begin
  self.Sort(@CompareMonServices);
end;

function CompareMonServicesSleep(Item1, Item2: Pointer): integer;
var
  s1, s2: TDateTime;
begin
  s1 := TMonService(Item1).LastEventTime;
  s2 := TMonService(Item2).LastEventTime;
  Result := 0;
  if s1 > s2 then
    Result := 1
  else if s1 < s2 then
    Result := -1;
end;

procedure TMonServiceList.SortBySleepTime();
begin
  self.Sort(@CompareMonServicesSleep);
end;

{ TMonService }

constructor TMonService.Create(AName: string);
begin
  self.Name := AName;
  self.FLog := TSysLog.Create();
  self.FLog.FileName := 'log/' + AName + '.log';
  self.FOptions := TStringList.Create();
  self.LastEventTime := Now();
  self.FAlertMuteKeywords := TStringList.Create();
end;

destructor TMonService.Destroy;
begin
  if Assigned(FChecker) then
    FreeAndNil(FChecker);
  FreeAndNil(FAlertMuteKeywords);
  FreeAndNil(FOptions);
  FreeAndNil(FLog);
  inherited Destroy;
end;

procedure TMonService.AddLogMessage(ASeverity: byte;
  const AHostname, AAppname, AText: string);
var
  sText: string;
begin
  sText := AText;
  if Self.Encoding <> '' then
  begin
    if Self.Encoding = 'CP-1251' then
      sText := CP1251ToUtf8(AText);
  end;
  self.FLog.AddRec(ASeverity, AHostname, AAppname, sText);
  self.Comment := sText;
  self.LastEventTime := Now();
  // update service status
  case ASeverity of
    0..2:
    begin
      self.State := 1; // critical
      self.ErrorCounter := 20;
      if Assigned(VoiceAlert) and (VoiceStrCritical <> '') then
      begin
        if CheckAlert(sText) then
          VoiceAlert.Speak(VoiceStrCritical);
      end;
    end;
    3..4:
    begin
      self.State := 2; // non-critical
      self.ErrorCounter := 10;
      if Assigned(VoiceAlert) and (VoiceStrWarning <> '') then
      begin
        if CheckAlert(sText) then
          VoiceAlert.Speak(VoiceStrWarning);
      end;
    end;
    5..7:
    begin
      if self.ErrorCounter > 0 then
        Dec(self.ErrorCounter)
      else
        self.State := 0; // normal
    end;
  end;
end;

procedure TMonService.SaveToConfig(AConf: TIniFile; ASection: string);
var
  i: integer;
begin
  AConf.EraseSection(ASection);
  AConf.WriteString(ASection, 'ServiceName', self.Name);
  AConf.WriteString(ASection, 'ServiceCaption', self.FullName);
  AConf.WriteString(ASection, 'ServiceGroup', self.Group);
  AConf.WriteString(ASection, 'AppName', self.AppName);
  AConf.WriteString(ASection, 'HostName', self.HostName);
  AConf.WriteString(ASection, 'IpAddr', self.IpAddr);
  AConf.WriteString(ASection, 'SyslogServer', self.SyslogServer);
  AConf.WriteString(ASection, 'Encoding', self.Encoding);
  AConf.WriteString(ASection, 'VoiceStrWarning', self.VoiceStrWarning);
  AConf.WriteString(ASection, 'VoiceStrCritical', self.VoiceStrCritical);
  // save voice alert ignore keywords
  for i := 0 to self.AlertMuteKeywords.Count - 1 do
  begin
    AConf.WriteString(ASection, 'MuteKeywords_' + IntToStr(i), self.AlertMuteKeywords[i]);
  end;
end;

procedure TMonService.LoadFromConfig(AConf: TIniFile; ASection: string);
var
  i: integer;
  s: string;
begin
  self.Name := AConf.ReadString(ASection, 'ServiceName', '');
  self.FullName := AConf.ReadString(ASection, 'ServiceCaption', '');
  self.Group := AConf.ReadString(ASection, 'ServiceGroup', '');
  self.AppName := AConf.ReadString(ASection, 'AppName', '');
  self.HostName := AConf.ReadString(ASection, 'HostName', '');
  self.IpAddr := AConf.ReadString(ASection, 'IpAddr', '');
  self.SyslogServer := AConf.ReadString(ASection, 'SyslogServer', '');
  self.Encoding := AConf.ReadString(ASection, 'Encoding', '');
  self.VoiceStrWarning := AConf.ReadString(ASection, 'VoiceStrWarning', '');
  self.VoiceStrCritical := AConf.ReadString(ASection, 'VoiceStrCritical', '');
  if self.FullName = '' then
    self.FullName := self.Name;
  // read voice alert ignore keywords
  for i := 0 to 100 do
  begin
    s := AConf.ReadString(ASection, 'MuteKeywords_' + IntToStr(i), '');
    if Length(s) > 0 then
      self.AlertMuteKeywords.Append(s);
  end;
end;

procedure TMonService.OnCheckerHandler(Sender: TObject);
var
  Severity: byte;
  n: integer;
  rec: TLogRecord;
begin
  Severity := 6;
  if Sender = Checker then
  begin
    self.State := Checker.ResultCode;
    self.Comment := Checker.Comment;
    self.LastEventTime := Now();
    if Checker.ResultCode = 0 then
      Severity := 6
    else if Checker.ResultCode = 1 then
      Severity := 1
    else if Checker.ResultCode = 2 then
      Severity := 4;
    n := self.Log.AddRec(Severity, '', self.Name, Checker.Comment);
    {if (n >= 0) and (Length(self.SyslogServer) > 0) then
    begin
      if self.Log.GetItemRec(n, rec) then
        Logger.SendLogRec(rec, self.SyslogServer);
    end;}
  end;
end;

procedure TMonService.SetChecker(Value: TChecker);
begin
  if Assigned(FChecker) then
  begin
    FChecker.Free();
  end;
  FChecker := Value;
  if Assigned(FChecker) then
  begin
    FChecker.OnChecker := @OnCheckerHandler;
    FChecker.Start();
  end;
end;

function TMonService.CheckAlert(const AText: string): boolean;
var
  i: integer;
  s: string;
begin
  Result := True;
  for i := 0 to self.AlertMuteKeywords.Count - 1 do
  begin
    s := self.AlertMuteKeywords[i];
    if Pos(s, AText) > 0 then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TMonService.SetVoiceAlert(AValue: TVoiceAlert);
begin
  if FVoiceAlert = AValue then Exit;
  FVoiceAlert := AValue;
end;

{ TPingChecker }

procedure TPingChecker.Execute();
var
  Ping: TPingSend;
  PingResult: boolean;
begin
  //self.FreeOnTerminate := True;
  while not Terminated do
  begin
    self.Active := True;
    Ping := TPingSend.Create();
    try
      PingResult := Ping.Ping(self.IPAddr);
      if PingResult then
      begin
        self.ResultCode := 0;
        self.Comment := 'Ping: ' + IntToStr(Ping.PingTime);
        // check warning
        if Ping.PingTime > self.WarningValue then
        begin
          FOKs := 0;
          Inc(FWarnings);
          if FWarnings >= self.WarningCount then
            self.ResultCode := 2;
          // check error
          if Ping.PingTime > self.ErrorValue then
          begin
            Inc(FErrors);
            if FErrors >= self.ErrorCount then
              self.ResultCode := 1;
          end;
        end
        else
        begin
          // ok
          Inc(FOKs);
          if FOKs > 999999 then
            FOKs := 9;
          // reset error
          if (self.ResultCode = 1) and (FOKs >= self.ErrorResetCount) then
          begin
            FErrors := 0;
            Self.ResultCode := 2;
          end;
          // reset warning
          if (self.ResultCode = 2) and (FOKs >= self.WarningResetCount) then
          begin
            FWarnings := 0;
            Self.ResultCode := 0;
          end;
        end;
      end;
    finally
      FreeAndNil(Ping);
    end;
    Synchronize(@SyncProc);
    self.Active := False;
    // sleep for some time
    self.PingIntervalValue := 0;
    while (not Terminated) and (self.PingIntervalValue < self.PingInterval) do
    begin
      Inc(self.PingIntervalValue);
      self.Suspended := True;
    end;
  end;
  self.Terminate();
end;

constructor TPingChecker.Create(const AIPAddr: string);
begin
  inherited Create(True);
  self.IPAddr := AIPAddr;
  self.ResultCode := 1;
  self.WarningValue := 1000;
  self.WarningCount := 2;
  self.WarningResetCount := 1;
  self.ErrorValue := 5000;
  self.ErrorCount := 2;
  self.ErrorResetCount := 1;
  self.PingIntervalValue := 0;
end;


end.
