unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, TGC.Client,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  TGC.Handler, TGC.Handler.UpdateAuthorizationState, FMX.StdCtrls, FMX.Edit,
  FMX.Objects;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    TelegramClient1: TTelegramClient;
    ButtonGetMe: TButton;
    ButtonSendMessage: TButton;
    ButtonAuth: TButton;
    EditNumber: TEdit;
    Edit1: TEdit;
    LabelFormat: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TelegramClient1NeedAuthCode(Sender: TObject);
    procedure TelegramClient1AuthReady(Sender: TObject);
    procedure TelegramClient1NeedAuthPassword(Sender: TObject);
    procedure TelegramClient1Receive(Sender: TObject; const Data: string);
    procedure TelegramClient1Registration(Sender: TObject; const Terms: TTermsOfService);
    procedure TelegramClient1Error(Sender: TObject; const Code: Integer; const Message: string);
    procedure TelegramClient1NeedAuthConfirm(Sender: TObject; const Link: string);
    procedure ButtonGetMeClick(Sender: TObject);
    procedure TelegramClient1Close(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSendMessageClick(Sender: TObject);
    procedure ButtonAuthClick(Sender: TObject);
    procedure Edit1Enter(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit1ChangeTracking(Sender: TObject);
  public
  end;

var
  Form4: TForm4;

implementation

uses
  FMX.DialogService, TGC.Entity.User, System.JSON, TGC.Classes, System.DateUtils,
  TGC.Builder.SendMessage, TGC.Entity.Message, TGC.Builder.GetMe,
  TGC.Builder.SendMessageAlbum;

{$R *.fmx}

procedure TForm4.ButtonGetMeClick(Sender: TObject);
begin
  TelegramClient1.Methods.GetMe(
    procedure(User: TtgUser)
    begin
      Memo1.Lines.Add('TelegramClient1.Methods.GetMe callback'#13#10 + User.FirstName + ' ' + User.LastName);
    end);

  TelegramClient1.Methods.Execute(TBuildGetMe.Create, '',
    procedure(User: TJSONObject)
    begin
      Memo1.Lines.Add('TelegramClient1.Methods.GetMe callback'#13#10 + User.Format);
    end);
end;

procedure TForm4.ButtonSendMessageClick(Sender: TObject);
begin
  if not TelegramClient1.IsInitialized then
    Exit;     {
  TelegramClient1.Methods.SendMessage(
    TSendMessage.Create.InputMessageContent(
      TInputMessageText.Create.Text(TFormattedText.Create.Text('😁'))
    ).ChatId(1288857534) //268284944
     .Options(TMessageSendOptions.Create.SchedulingState(TMessageSchedulingStateSendAtDate.Create.SendDate(Now.IncMinute(2)))),
    procedure(Msg: TtgMessage)
    begin
      // сообщение отправлено
      Memo1.Lines.Add('sended msg');
    end);    }

  TelegramClient1.Methods.SendMessageAlbum(
    TSendMessageAlbum.Create.InputMessageContents([
    TInputMessagePhoto.Create.Photo(TInputFileLocal.Create.Path('D:\Temp\Photos\299990769.jpg')),
    TInputMessagePhoto.Create.Photo(TInputFileLocal.Create.Path('D:\Temp\Photos\299990763.jpg'))
    ]).ChatId(1288857534),
    procedure(Msg: TtgMessage)
    begin
      if Msg.IsError then
        Memo1.Lines.Add(Msg.Message)
      else
        Memo1.Lines.Add('sended msg');
    end);
     {
  TelegramClient1.Methods.SendMessage(
    TSendMessage.Create.InputMessageContent(
      TInputMessageDocument.Create.Document(TInputFileLocal.Create.Path('D:\Temp\Iconion\HGM\Material Icons_e80e(0)_128.png'))
    ).ChatId(1288857534),
    procedure(Msg: TtgMessage)
    begin
      // сообщение отправлено
      Memo1.Lines.Add('sended msg');
    end);   }
end;

procedure TForm4.Edit1ChangeTracking(Sender: TObject);
begin
  var Date: TDateTime;
  if TryStrToDateTime(Edit1.Text, Date) then
    LabelFormat.Text := FormatDateTime('dd mmmm yyyy г.', Date)
  else
    LabelFormat.Text := 'Не корректная дата';
end;

procedure TForm4.Edit1Enter(Sender: TObject);
begin
  LabelFormat.Visible := False;
  Edit1.FontColor := TAlphaColorRec.Black;
end;

procedure TForm4.Edit1Exit(Sender: TObject);
begin
  LabelFormat.Visible := True;
  Edit1.FontColor := TAlphaColorRec.Null;
end;

procedure TForm4.ButtonAuthClick(Sender: TObject);
begin
  TelegramClient1.PhoneNumber := EditNumber.Text;
  if not TelegramClient1.Initializate then
    ShowMessage('Not inited');
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  //
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  TThread.RemoveQueuedEvents(nil);
end;

procedure TForm4.TelegramClient1AuthReady(Sender: TObject);
begin
  //ShowMessage('ready');
end;

procedure TForm4.TelegramClient1Close(Sender: TObject);
begin
  Memo1.Lines.Add('Client closed. Recreating');
  TelegramClient1.Initializate;
end;

procedure TForm4.TelegramClient1Error(Sender: TObject; const Code: Integer; const Message: string);
begin
  ShowMessage('Error: ' + Message + #13#10' Code: ' + Code.ToString);
end;

procedure TForm4.TelegramClient1NeedAuthCode(Sender: TObject);
begin
  TDialogService.InputQuery('User Authorization', ['Enter the authorization code'], [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk then
        TelegramClient1.SetAuthCode(AValues[0]);
    end);
end;

procedure TForm4.TelegramClient1NeedAuthConfirm(Sender: TObject; const Link: string);
begin
  ShowMessage('Confirm ' + Link);
end;

procedure TForm4.TelegramClient1NeedAuthPassword(Sender: TObject);
begin
  TDialogService.InputQuery('User Authentication', ['Enter the access code (password)'], [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk then
        TelegramClient1.SetAuthPassword(AValues[0]);
    end);
end;

procedure TForm4.TelegramClient1Receive(Sender: TObject; const Data: string);
begin
  Memo1.Lines.Add(Data);
end;

procedure TForm4.TelegramClient1Registration(Sender: TObject; const Terms: TTermsOfService);
begin
  TDialogService.InputQuery('User Registration', ['First Name', 'Last Name'], ['', ''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk then
        TelegramClient1.SetRegisterUser(AValues[0], AValues[1]);
    end);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

