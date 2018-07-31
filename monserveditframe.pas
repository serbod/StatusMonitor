unit MonServEditFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, services;

type

  { TFrameMonServEdit }

  TFrameMonServEdit = class(TFrame)
    btnOK: TButton;
    btnCancel: TButton;
    cbGroup: TComboBox;
    cbEncoding: TComboBox;
    edVoiceStrCritical: TEdit;
    edIpAddr: TEdit;
    edFullName: TEdit;
    edHostName: TEdit;
    edAppName: TEdit;
    edVoiceStrWarning: TEdit;
    edName: TEdit;
    gbMonServProps: TGroupBox;
    gbMuteKeywords: TGroupBox;
    gbVoiceAlerts: TGroupBox;
    lbEncoding: TLabel;
    lbVoiceStrWarning: TLabel;
    lbVoiceStrCritical: TLabel;
    lbAppName: TLabel;
    lbIpAddr: TLabel;
    lbHostName: TLabel;
    lbGroup: TLabel;
    lbFullName: TLabel;
    lbName: TLabel;
    MemoMuteKeywords: TMemo;
    panLeft: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    MonServ: TMonService;
    procedure UpdateView();
    procedure Save();
  end;

implementation

{$R *.lfm}

{ TFrameMonServEdit }

procedure TFrameMonServEdit.btnOKClick(Sender: TObject);
begin
  Save();
  if Assigned(Parent) and (Parent is TForm) then
    (Parent as TForm).Release();
end;

procedure TFrameMonServEdit.btnCancelClick(Sender: TObject);
begin
  if Assigned(Parent) and (Parent is TForm) then
    (Parent as TForm).Release();
end;

procedure TFrameMonServEdit.UpdateView();
begin
  if not Assigned(MonServ) then
    Exit;
  edName.Text := MonServ.Name;
  edFullName.Text := MonServ.FullName;
  cbGroup.Text := MonServ.Group;
  edAppName.Text := MonServ.AppName;
  edHostName.Text := MonServ.HostName;
  edIpAddr.Text := MonServ.IpAddr;
  edVoiceStrWarning.Text := MonServ.VoiceStrWarning;
  edVoiceStrCritical.Text := MonServ.VoiceStrCritical;
  cbEncoding.Text := MonServ.Encoding;
  MemoMuteKeywords.Lines.Assign(MonServ.AlertMuteKeywords);
end;

procedure TFrameMonServEdit.Save();
begin
  if not Assigned(MonServ) then
    Exit;
  MonServ.FullName := Trim(edFullName.Text);
  MonServ.Group := Trim(cbGroup.Text);
  MonServ.AppName := Trim(edAppName.Text);
  MonServ.HostName := Trim(edHostName.Text);
  MonServ.IpAddr := Trim(edIpAddr.Text);
  MonServ.VoiceStrWarning := Trim(edVoiceStrWarning.Text);
  MonServ.VoiceStrCritical := Trim(edVoiceStrCritical.Text);
  MonServ.Encoding := cbEncoding.Text;
  MonServ.AlertMuteKeywords.Assign(MemoMuteKeywords.Lines);
end;

end.
