unit log_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls,
  ExtCtrls, ActnList, Menus, log_service, Logger, Dialogs, Clipbrd,
  Graphics, VirtualTrees;

const
  ERROR_COLOR   = $00BBBBFF;
  WARNING_COLOR = $0000FFFF;
  NOTE_COLOR    = $0088FFFF;
type

  { TLogBrowserFrame }
  { Фрейм журнала событий }
  TLogBrowserFrame = class(TFrame)
    actSaveToAddr: TAction;
    actSaveToClipboard: TAction;
    actSaveToFile: TAction;
    alLog: TActionList;
    btnSave: TButton;
    btnUpdate: TButton;
    btnLoad: TButton;
    btnClearLog: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    pmLog: TPopupMenu;
    tmrUpdate: TTimer;
    cbAutoUpdate: TToggleBox;
    VST: TVirtualStringTree;
    procedure actSaveToClipboardExecute(Sender: TObject);
    procedure actSaveToFileExecute(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure cbAutoUpdateChange(Sender: TObject);
    procedure lvLogCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvLogData(Sender: TObject; Item: TListItem);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    { private declarations }
  public
    { public declarations }
    Log: TSysLog;
    procedure UpdateLogItem(LogItem: ILogItem; li: TListItem);
    procedure UpdateLog();
    procedure AfterConstruction; override;
  end;

  PTreeData = ^TTreeData;
  TTreeData = record
    Item: TLogItem;
  end;

implementation

{$R *.lfm}

{
Значимость    80
Дата, время   100
Адрес         80
Программа     100
Сообщение     400
}

{ TLogBrowserFrame }

procedure TLogBrowserFrame.btnUpdateClick(Sender: TObject);
begin
  UpdateLog();
end;

procedure TLogBrowserFrame.cbAutoUpdateChange(Sender: TObject);
begin
  tmrUpdate.Enabled := cbAutoUpdate.Checked;
end;

procedure TLogBrowserFrame.lvLogCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  LogItem: ILogItem;
  BgColor: TColor;
  n: integer;
begin
  BgColor := clWindow;
  n := Item.Index;
  if n < Log.Count then
  begin
    LogItem := Log.GetItem(n);
    if Assigned(LogItem) then
    begin
      case LogItem.GetSeverity() of
        0..3: BgColor := ERROR_COLOR; // critical
        4..4: BgColor := WARNING_COLOR; // non-critical
        5..5: BgColor := NOTE_COLOR;
        6..7: BgColor := clWindow; // normal
      end;
    end;
  end;
  Sender.Canvas.Brush.Color := BgColor;
end;

procedure TLogBrowserFrame.lvLogData(Sender: TObject; Item: TListItem);
var
  n: integer;
  LogItem: ILogItem;
begin
  n := Item.Index;
  if n < Log.Count then
  begin
    LogItem := Log.GetItem(n);
    UpdateLogItem(LogItem, Item);
  end;
end;

procedure TLogBrowserFrame.tmrUpdateTimer(Sender: TObject);
begin
  UpdateLog();
end;

procedure TLogBrowserFrame.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  BgColor: TColor;
  LogItem: ILogItem;
begin
  DefaultDraw := True;
  LogItem := Log.GetItem(Node^.Index);
  BgColor := clWindow;
  if Assigned(LogItem) then
  begin
    case LogItem.GetSeverity() of
      0..3: BgColor := ERROR_COLOR; // critical
      4..4: BgColor := WARNING_COLOR; // non-critical
      5..5: BgColor := NOTE_COLOR;
      6..7: BgColor := clWindow; // normal
    else
    end;
  end;
  Sender.Canvas.Brush.Color := BgColor;
end;

procedure TLogBrowserFrame.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  LogItem: ILogItem;
begin
  LogItem := Log.GetItem(Node^.Index);
  if Column = 0 then
  begin
    case LogItem.GetSeverity() of
      0..3: ImageIndex := 1; // critical
      4..5: ImageIndex := 2; // non-critical
      6..7: ImageIndex := 0; // normal
    end;
  end;
end;

procedure TLogBrowserFrame.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TLogBrowserFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  LogRec: TLogRecord;
begin
  if not Log.GetItemRec(Node^.Index, LogRec) then Exit;
  case Column of
    // Значимость
    0: CellText := SeverityStr(LogRec);
    // Дата, время
    1: CellText := FormatDateTime('YYYY-MM-DD HH:MM:SS', LogRec.Timestamp);
    // Адрес
    2: CellText := LogRec.IpAddress + ' ' + LogRec.Hostname;
    // Программа
    3: CellText := LogRec.Appname;
    // Сообщение
    4: CellText := LogRec.Text;
  end;
end;

procedure TLogBrowserFrame.UpdateLogItem(LogItem: ILogItem; li: TListItem);
var
  LogRec: TLogRecord;
begin
  LogItem.GetLogRec(LogRec);
  //li.Data := LogItem;
  li.Caption := SeverityStr(LogRec);
  case LogRec.Severity of
    0..3: li.ImageIndex := 1; // critical
    4..5: li.ImageIndex := 2; // non-critical
    6..7: li.ImageIndex := 0; // normal
  end;
  li.SubItems.Clear();
  li.SubItems.Add(FormatDateTime('YYYY-MM-DD HH:MM:SS', LogRec.Timestamp));
  li.SubItems.Add(LogRec.IpAddress + ' ' + LogRec.Hostname);
  li.SubItems.Add(LogRec.Appname);
  li.SubItems.Add(LogRec.Text);
end;

procedure TLogBrowserFrame.btnLoadClick(Sender: TObject);
begin
  Log.FromFile(Log.FileName);
  UpdateLog();
end;

procedure TLogBrowserFrame.btnClearLogClick(Sender: TObject);
begin
  Log.Clear();
  UpdateLog();
end;

procedure TLogBrowserFrame.actSaveToFileExecute(Sender: TObject);
var
  d: TSaveDialog;
begin
  d := TSaveDialog.Create(self);
  d.DefaultExt := 'log';
  try
    if d.Execute() then
    begin
      Log.ToFile(d.FileName);
    end;
  finally
    d.Free();
  end;
end;

procedure TLogBrowserFrame.actSaveToClipboardExecute(Sender: TObject);
var
  sl: TStringList;
  Node: PVirtualNode;
begin
  sl := TStringList.Create();
  try
    for Node in VST.SelectedNodes do
    begin
      sl.Add(Log.GetItemStr(Node^.Index));
    end;
    Clipboard.AsText := sl.Text;
  finally
    sl.Free();
  end;
end;

procedure TLogBrowserFrame.UpdateLog();
var
  iOffsY: Integer;
  IsNeedScroll: Boolean;
  //i: Integer;
  //VSTNode: PVirtualNode; }
begin
  IsNeedScroll := (VST.FocusedNode = VST.GetLast());

  //iOffsY := VST.OffsetY;
  if Cardinal(Log.Count) <> VST.RootNodeCount then
  begin
    while Cardinal(Log.Count) > VST.RootNodeCount do
    begin
      VST.AddChild(nil);
      //IsNeedScroll := True;
    end;
    while Cardinal(Log.Count) < VST.RootNodeCount do
    begin
      VST.DeleteNode(VST.GetLast());
    end;
  end;
  VST.Invalidate();

  {VST.BeginUpdate;
  VST.Clear;
  for i := 0 to Log.Count-1 do
  begin
    VSTNode := VST.AddChild(nil);
  end;
  VST.EndUpdate;}

  //VST.OffsetY := iOffsY;
  if IsNeedScroll then
    VST.FocusedNode := VST.GetLast();
  //PostMessage(VST.Handle, WM_VSCROLL, SB_BOTTOM, 0);

  tmrUpdate.Enabled := cbAutoUpdate.Checked;
end;

procedure TLogBrowserFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  self.DoubleBuffered := True;
end;

end.
