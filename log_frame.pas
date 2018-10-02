unit log_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls,
  ExtCtrls, ActnList, Menus, log_service, Logger, Dialogs, Clipbrd,
  Graphics, Buttons, VirtualTrees;

const
  ERROR_COLOR   = $00BBBBFF;
  WARNING_COLOR = $0000FFFF;
  NOTE_COLOR    = $0088FFFF;
type

  { TLogBrowserFrame }
  { Фрейм журнала событий }
  TLogBrowserFrame = class(TFrame)
    actFreezeLog: TAction;
    actClearLog: TAction;
    actLoadFromFile: TAction;
    actSaveToAddr: TAction;
    actSaveToClipboard: TAction;
    actSaveToFile: TAction;
    alLog: TActionList;
    btnUpdate: TButton;
    btnClearLog: TButton;
    cbFreezelog: TCheckBox;
    edFilterStr: TEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    pmLog: TPopupMenu;
    tmrUpdate: TTimer;
    VST: TVirtualStringTree;
    procedure actClearLogExecute(Sender: TObject);
    procedure actFreezeLogExecute(Sender: TObject);
    procedure actLoadFromFileExecute(Sender: TObject);
    procedure actSaveToClipboardExecute(Sender: TObject);
    procedure actSaveToFileExecute(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure edFilterStrChange(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    { private declarations }
  public
    { public declarations }
    Log: TSysLog;
    procedure UpdateLog();
    procedure AfterConstruction; override;
  end;

  {PTreeData = ^TTreeData;
  TTreeData = record
    ItemIndex: Integer;
  end; }

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

procedure TLogBrowserFrame.edFilterStrChange(Sender: TObject);
begin
  Log.FilterStr := edFilterStr.Text;
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
  ll: TLogLevel;
begin
  DefaultDraw := True;
  ll := Log.GetItemLoglevel(Node^.Index);
  case Ord(ll) of
    0..3: BgColor := ERROR_COLOR; // critical
    4..4: BgColor := WARNING_COLOR; // non-critical
    5..5: BgColor := NOTE_COLOR;
    6..7: BgColor := clWindow; // normal
  else
    BgColor := clWindow;
  end;
  Sender.Canvas.Brush.Color := BgColor;
end;

procedure TLogBrowserFrame.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  ll: TLogLevel;
begin
  if Column = 0 then
  begin
    ll := Log.GetItemLoglevel(Node^.Index);
    case Ord(ll) of
      0..3: ImageIndex := 1; // critical
      4..5: ImageIndex := 2; // non-critical
      6..7: ImageIndex := 0; // normal
    end;
  end;
end;

procedure TLogBrowserFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  //LogRec: TLogRecord;
  LogItem: TLogItem;
begin
  if Log.RWLock.BeginRead() then
  begin
    LogItem := Log.GetItem(Node^.Index);
    if Assigned(LogItem) then
    begin
      case Column of
        // Значимость
        0: CellText := LLToStr(LogItem.LogLevel);
        // Дата, время
        1: CellText := FormatDateTime('YYYY-MM-DD HH:MM:SS', LogItem.Timestamp);
        // Адрес
        2:
        begin
          CellText := '';
          if LogItem.IpAddress <> 0 then
            CellText := LongWordToIPStr(LogItem.IpAddress) + ' ';

          CellText := CellText + Log.GetHostnameByIndex(LogItem.HostnameID);
        end;
        // Программа
        3: CellText := Log.GetAppnameByIndex(LogItem.AppnameID);
        // Сообщение
        4: CellText := LogItem.Text;
      end;

    end;
    Log.RWLock.EndRead();
  end;
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

procedure TLogBrowserFrame.actFreezeLogExecute(Sender: TObject);
begin
  Log.Freeze := not Log.Freeze;
  actFreezeLog.Checked := Log.Freeze;
end;

procedure TLogBrowserFrame.actLoadFromFileExecute(Sender: TObject);
begin
  Log.FromFile(Log.FileName);
  UpdateLog();
end;

procedure TLogBrowserFrame.actClearLogExecute(Sender: TObject);
begin
  Log.Clear();
  UpdateLog();
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

  //tmrUpdate.Enabled := True;
end;

procedure TLogBrowserFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  self.DoubleBuffered := True;
end;

end.
