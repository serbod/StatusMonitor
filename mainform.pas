unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ActnList, StdCtrls, Buttons, ComCtrls, services,
  log_service, IniFiles, contnrs, sysinfo;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actDatabase: TAction;
    actInternet: TAction;
    actCanteen: TAction;
    actInfoTerminal: TAction;
    act: TAction;
    actDeleteService: TAction;
    actEditService: TAction;
    actSaveServices: TAction;
    alServices: TActionList;
    actPayTerminal: TAction;
    actSecurityPost: TAction;
    actMainServer: TAction;
    actNetwork: TAction;
    alTray: TActionList;
    bbtnDefaultLog: TBitBtn;
    btnConfig: TButton;
    ImgListIcons: TImageList;
    lbIpAddr: TLabel;
    lvServices: TListView;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pmTray: TPopupMenu;
    pmServices: TPopupMenu;
    tmrTick: TTimer;
    ToolButton2: TToolButton;
    TrayIcon: TTrayIcon;
    procedure actDeleteServiceExecute(Sender: TObject);
    procedure actEditMuteKeywordsExecute(Sender: TObject);
    procedure actEditServiceExecute(Sender: TObject);
    procedure actExecute(Sender: TObject);
    procedure actRenameGroupExecute(Sender: TObject);
    procedure actRenameServiceExecute(Sender: TObject);
    procedure actSaveServicesExecute(Sender: TObject);
    procedure bbtnDefaultLogClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvServicesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvServicesDblClick(Sender: TObject);
    procedure lvServicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure MenuItem14Click(Sender: TObject);
    procedure tmrTickTimer(Sender: TObject);
  private
    { private declarations }
    FFormsList: TComponentList;
    FConfigFileName: string;
    FMonServiceList: TMonServiceList;
    FSysLogServer: TSysLogServer;
    FSelectedService: TMonService;
    FVoiceAlert: TVoiceAlert;
  public
    { public declarations }
    MinimizeOnExit: boolean;
    property MonServiceList: TMonServiceList read FMonServiceList;
    property SysLogServer: TSysLogServer read FSysLogServer;
    property SelectedService: TMonService read FSelectedService;
    property VoiceAlert: TVoiceAlert read FVoiceAlert;
    procedure OpenLogBrowser(ALog: TSysLog; ACaption: string);
    procedure OpenLogServiceEdit(AMonService: TMonService);
  end;

var
  frmMain: TfrmMain;

const
  cOkIcon = 0;    // Все в порядке
  cFailIcon = 1;  // Не работает
  cErrIcon = 2;   // Работает с ошибками

implementation

uses log_frame, MonServEditFrame, ConfigForm;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
  conf: TMemIniFile;
  s, SectName, ServName, ServCaption: string;
  Ping: TPingChecker;
  ConsoleChecker: TConsoleChecker;
  MonSer: TMonService;
begin
  FFormsList := TComponentList.Create();
  FMonServiceList := TMonServiceList.Create(True);

  lbIpAddr.Caption := sysinfo.GetIpAddrList();

  FConfigFileName := ExtractFileNameWithoutExt(ParamStr(0)) + '.ini';
  conf := TMemIniFile.Create(FConfigFileName);
  try

    // main form
    MinimizeOnExit := conf.ReadBool('MainForm', 'MinimizeOnExit', True);
    FVoiceAlert := nil;
    if conf.ReadBool('MainForm', 'VoiceAlert', False) then
      FVoiceAlert := TVoiceAlert.Create();
    // window position
    self.Top := 10;
    self.Left := 10;

    // pingers
    for i := 1 to 20 do
    begin
      SectName := 'Pingers.Pinger_' + IntToStr(i);
      if not conf.SectionExists(SectName) then
        Break;

      ServName := conf.ReadString(SectName, 'ServiceName', '');
      if ServName = '' then
        Continue;
      ServCaption := conf.ReadString(SectName, 'ServiceCaption', '');
      if ServCaption = '' then
        ServCaption := ServName;

      s := conf.ReadString(SectName, 'PingAddr', '');
      if s = '' then
        Continue;
      Ping := TPingChecker.Create(s);
      Ping.PingInterval := conf.ReadInteger(SectName, 'PingInterval', Ping.PingInterval);
      Ping.WarningValue := conf.ReadInteger(SectName, 'WarningValue', Ping.WarningValue);
      Ping.WarningCount := conf.ReadInteger(SectName, 'WarningCount', Ping.WarningCount);
      Ping.WarningResetCount := conf.ReadInteger(SectName, 'WarningResetCount', Ping.WarningResetCount);
      Ping.ErrorValue := conf.ReadInteger(SectName, 'ErrorValue', Ping.ErrorValue);
      Ping.ErrorCount := conf.ReadInteger(SectName, 'ErrorCount', Ping.ErrorCount);
      Ping.ErrorResetCount := conf.ReadInteger(SectName, 'ErrorResetCount', Ping.ErrorResetCount);

      MonSer := MonServiceList.AddService(ServName, ServCaption, Ping);
      MonSer.Log.WriteBufferDepth := -1; // not write log
      MonSer.SyslogServer := conf.ReadString(SectName, 'SyslogServer', '');
    end;

    // loggers
    for i := 1 to 200 do
    begin
      SectName := 'Loggers.Logger_' + IntToStr(i);
      if not conf.SectionExists(SectName) then
        Break;

      ServName := conf.ReadString(SectName, 'ServiceName', '');
      if ServName = '' then
        Continue;
      MonSer := MonServiceList.AddService(ServName, ServName, nil);
      MonSer.LoadFromConfig(conf, SectName);
      Monser.VoiceAlert := VoiceAlert;
    end;

    // console checkers
    for i := 1 to 20 do
    begin
      SectName := 'ConsoleCheckers.ConsoleChecker_' + IntToStr(i);
      if not conf.SectionExists(SectName) then
        Break;

      ServName := conf.ReadString(SectName, 'ServiceName', '');
      if ServName = '' then
        Continue;
      ServCaption := conf.ReadString(SectName, 'ServiceCaption', '');
      if ServCaption = '' then
        ServCaption := ServName;

      s := conf.ReadString(SectName, 'CommandLine', '');
      if s = '' then
        Continue;
      ConsoleChecker := TConsoleChecker.Create(s);

      MonSer := MonServiceList.AddService(ServName, ServCaption, ConsoleChecker);
      MonSer.SyslogServer := conf.ReadString(SectName, 'SyslogServer', '');
    end;

    // Syslog server
    SectName := 'SyslogServer';
    FSysLogServer := TSysLogServer.Create(MonServiceList);
    FSysLogServer.UplinkServer := conf.ReadString(SectName, 'UplinkServer', '');
    FSysLogServer.UplinkServer2 := conf.ReadString(SectName, 'UplinkServer2', '');
    FSysLogServer.AutoCreateLoggers := conf.ReadBool(SectName, 'AutoCreateLoggers', True);
    FSysLogServer.DefaultEncoding := conf.ReadString(SectName, 'DefaultEncoding', '');
    FSysLogServer.VoiceStrWarning := conf.ReadString(SectName, 'VoiceStrWarning', 'Attention!');
    FSysLogServer.VoiceStrCritical := conf.ReadString(SectName, 'VoiceStrCritical', 'Alarm! Alarm!');
    FSysLogServer.VoiceStrNewService := conf.ReadString(SectName, 'VoiceStrNewService', 'New service detected!');
    FSysLogServer.VoiceAlert := VoiceAlert;
    // App names
    for i := 1 to 20 do
    begin
      ServName := conf.ReadString(SectName, 'AppName_' + IntToStr(i), '');
      if ServName = '' then
        Break;
      ServCaption := conf.ReadString(SectName, 'AppCaption_' + IntToStr(i), '');
      SysLogServer.AppNames.Values[ServName] := ServCaption;
    end;
    // Alert mute keywords
    for i := 1 to 50 do
    begin
      s := conf.ReadString(SectName, 'AlertMute_' + IntToStr(i), '');
      if s = '' then
        Break;
      SysLogServer.AlertMuteKeywords.Add(s);
    end;

  finally
    conf.Free();
  end;

  //MonServiceList.AddService('database', 'База данных', TPingChecker.Create('192.168.1.100'));
  //MonServiceList.AddService('lan', 'Локальная сеть', TPingChecker.Create('192.168.1.1'));
  //MonServiceList.AddService('internet', 'Интернет', TPingChecker.Create('8.8.8.8'));
  //MonServiceList.AddService('main_server', 'Главный сервер', TPingChecker.Create('cab.classcard.ru'));
  //MonServiceList.AddService('KassaLite', 'АРМ кассира столовой', nil);
  //MonServiceList.AddService('nettop-1', 'Неттоп', nil);

  //ImgListIcons.GetIcon(cOkIcon, TrayIcon.Icon);
  TrayIcon.Icon := frmMain.Icon;
  ImgListIcons.GetBitmap(5, bbtnDefaultLog.Glyph);

  lvServices.DoubleBuffered := True;
  // Fill list
  tmrTickTimer(self);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMonServiceList);
  if Assigned(FSysLogServer) then
    FreeAndNil(FSysLogServer);
  if Assigned(FVoiceAlert) then
    FreeAndNil(FVoiceAlert);
  FreeAndNil(FFormsList);
end;

procedure TfrmMain.lvServicesColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index = 0 then
    MonServiceList.SortByGroup();
  if Column.Index = 2 then
    MonServiceList.SortBySleepTime();
end;

procedure TfrmMain.lvServicesDblClick(Sender: TObject);
begin
  if not Assigned(SelectedService) then
    Exit;
  OpenLogBrowser(SelectedService.Log, SelectedService.FullName);
end;

procedure TfrmMain.lvServicesSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
var
  mons: TMonService;
begin
  if not Selected then
  begin
    FSelectedService := nil;
    Exit;
  end;
  if not Assigned(Item) then
    Exit;
  mons := TMonService(Item.Data);
  if not Assigned(mons) then
    Exit;
  FSelectedService := mons;
  //OpenLogBrowser(mons.Log, mons.FullName);
end;

procedure TfrmMain.MenuItem14Click(Sender: TObject);
begin
  //VoiceAlert.AddText('Warning! Warning! Something bad happening!');
  if Assigned(VoiceAlert) then
    VoiceAlert.Speak('Warning! Something bad happening!');
end;

procedure TfrmMain.tmrTickTimer(Sender: TObject);
var
  i: integer;
  li: TListItem;
  mons: TMonService;
  //sl: TStringList;
  dt_now: TDateTime;
begin
  dt_now := Now();
  // Update state
  //lvServices.BeginUpdate();  // почему-то это не помогает от мерцания, только хуже
  //lvServices.Items.Clear();
  for i := 0 to MonServiceList.Count - 1 do
  begin
    if i >= lvServices.Items.Count then
      li := lvServices.Items.Add()
    else
      li := lvServices.Items.Item[i];
    mons := (MonServiceList[i] as TMonService);
    li.Caption := mons.Group;
    li.ImageIndex := mons.State;
    li.Data := mons;

    // для устранения мерцания
    //li.SubItems.Clear();
    // Full name
    if li.SubItems.Count < 1 then
      li.SubItems.Append('');
    li.SubItems[0] := mons.FullName;
    // Sleep time
    if li.SubItems.Count < 2 then
      li.SubItems.Append('');
    if dt_now - mons.LastEventTime < 1 then
      li.SubItems[1] := FormatDateTime('HH:MM:SS', dt_now - mons.LastEventTime)
    else
      li.SubItems[1] := IntToStr(Trunc(dt_now - mons.LastEventTime)) + ' суток';
    // Comment
    if li.SubItems.Count < 3 then
      li.SubItems.Append('');
    li.SubItems[2] := mons.Comment;

    // Re-check
    if Assigned(mons.Checker) then
    begin
      if not mons.Checker.Active then
        mons.Checker.Suspended := False;
      {
      // test console messages (!!!)
      if (mons.Checker is TConsoleChecker) then
      begin
        sl:=TStringList.Create();
        sl.Text:=(mons.Checker as TConsoleChecker).ReadConsole();
        for n:=0 to sl.Count-1 do
        begin
          mons.Log.AddRec(6, '', '', sl[n]);
        end;
        sl.Free();
      end;
      }
    end;

    // tick loggers
    mons.Log.Tick();
  end;
  // delete unused lines
  for i := lvServices.Items.Count - 1 downto MonServiceList.Count do
    lvServices.Items.Delete(i);
  //lvServices.EndUpdate();

  // update IP every hour
  if (lbIpAddr.Caption = '') and (FormatDateTime('nn', dt_now) = '00') then
    lbIpAddr.Caption := sysinfo.GetIpAddrList();
end;

procedure TfrmMain.OpenLogBrowser(ALog: TSysLog; ACaption: string);
var
  LogForm: TForm;
  LogFrame: TLogBrowserFrame;
  n: integer;
begin
  LogForm := nil;
  if Assigned(ALog.Control) then
  begin
    n := FFormsList.IndexOf(ALog.Control);
    if n >= 0 then
      LogForm := (ALog.Control as TForm);
  end;
  if not Assigned(LogForm) then
  begin
    LogForm := TForm.Create(frmMain);
    LogFrame := TLogBrowserFrame.Create(LogForm);
    LogFrame.Log := ALog;
    LogForm.Height := LogFrame.Height;
    LogForm.Width := LogFrame.Width;
    LogFrame.Parent := LogForm;
    LogFrame.Align := alClient;
    LogForm.Caption := ACaption;
    ALog.Control := LogForm;
    FFormsList.Add(LogForm);
  end;
  LogForm.Show();
end;

procedure TfrmMain.OpenLogServiceEdit(AMonService: TMonService);
var
  Form: TForm;
  Frame: TFrameMonServEdit;
  i: integer;
  s: string;
begin
  if not Assigned(AMonService) then
    Exit;
  Form := TForm.Create(self);
  Frame := TFrameMonServEdit.Create(Form);
  Form.Caption := AMonService.FullName;
  Form.Width := Frame.Width;
  Form.Height := Frame.Height;
  Frame.Parent := Form;
  Frame.Align := alClient;
  // fill groups list
  for i := 0 to MonServiceList.Count - 1 do
  begin
    s := Trim(MonServiceList.GetByIndex(i).Group);
    if Frame.cbGroup.Items.IndexOf(s) < 0 then
      Frame.cbGroup.Items.Append(s);
  end;
  Frame.MonServ := AMonService;
  Frame.UpdateView();
  Form.ShowOnTop();
end;


procedure TfrmMain.actExecute(Sender: TObject);
begin

end;

procedure TfrmMain.actRenameGroupExecute(Sender: TObject);
var
  sName: string;
begin
  if not Assigned(SelectedService) then
    Exit;
  sName := SelectedService.Group;
  if Dialogs.InputQuery('Укажите название группы', 'Название:', sName) then
  begin
    SelectedService.Group := sName;
    MonServiceList.SortByGroup();
  end;
end;

procedure TfrmMain.actDeleteServiceExecute(Sender: TObject);
begin
  if not Assigned(SelectedService) then
    Exit;
  if Dialogs.QuestionDlg('Внимание!', 'Убрать сервис из списка?',
    mtConfirmation, [mrYes, mrNo], '') = mrYes then
  begin
    MonServiceList.Remove(SelectedService);
  end;

end;

procedure TfrmMain.actEditMuteKeywordsExecute(Sender: TObject);
begin

end;

procedure TfrmMain.actEditServiceExecute(Sender: TObject);
begin
  if not Assigned(SelectedService) then
    Exit;
  OpenLogServiceEdit(SelectedService);
end;

procedure TfrmMain.actRenameServiceExecute(Sender: TObject);
var
  sName: string;
begin
  if not Assigned(SelectedService) then
    Exit;
  sName := SelectedService.FullName;
  if Dialogs.InputQuery('Укажите название сервиса', 'Название:', sName) then
  begin
    SelectedService.FullName := sName;
  end;

end;

procedure TfrmMain.actSaveServicesExecute(Sender: TObject);
var
  i, n: integer;
  s: string;
  Conf: TMemIniFile;
begin
  Conf := TMemIniFile.Create(FConfigFileName);
  n := 1;
  for i := 0 to MonServiceList.Count - 1 do
  begin
    if Assigned(MonServiceList.GetByIndex(i).Checker) then
      Continue;
    MonServiceList.GetByIndex(i).SaveToConfig(Conf, 'Loggers.Logger_' + IntToStr(n));
    Inc(n);
  end;
  // remove remaining sections
  for i := n to 200 do
  begin
    s := 'Loggers.Logger_' + IntToStr(i + 1);
    if Conf.SectionExists(s) then
      Conf.EraseSection(s);
  end;
  Conf.UpdateFile();
  Conf.Free();
end;

procedure TfrmMain.bbtnDefaultLogClick(Sender: TObject);
begin
  OpenLogBrowser(SysLogServer.DefaultLog, bbtnDefaultLog.Caption);
end;

procedure TfrmMain.btnConfigClick(Sender: TObject);
begin
  sConfIni := FConfigFileName;
  ShowConfigForm(nil, Self);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not (ssShift in GetKeyShiftState()) then
  begin
    if MinimizeOnExit then
      CanClose := False;
    Application.Minimize();
  end;
end;



end.
