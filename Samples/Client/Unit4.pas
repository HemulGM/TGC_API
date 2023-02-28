unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, TGC.Client,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  TGC.Handler, TGC.Handler.UpdateAuthorizationState, FMX.StdCtrls;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    TelegramClient1: TTelegramClient;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TelegramClient1NeedAuthCode(Sender: TObject);
    procedure TelegramClient1AuthReady(Sender: TObject);
    procedure TelegramClient1NeedAuthPassword(Sender: TObject);
    procedure TelegramClient1Receive(Sender: TObject; const Data: string);
    procedure TelegramClient1Registration(Sender: TObject; const Terms: TTermsOfService);
    procedure TelegramClient1Error(Sender: TObject; const Code: Integer; const Message: string);
    procedure TelegramClient1NeedAuthConfirm(Sender: TObject; const Link: string);
    procedure Button1Click(Sender: TObject);
    procedure TelegramClient1Close(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
  end;

var
  Form4: TForm4;

implementation

uses
  FMX.DialogService, TGC.Entity.User, System.JSON, TGC.Classes,
  TGC.Builder.SendMessage, TGC.Entity.Message;

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  TelegramClient1.Methods.GetMe(
    procedure(User: TtgUser)
    begin
      Memo1.Lines.Add('TelegramClient1.Methods.GetMe callback'#13#10 + User.FirstName + ' ' + User.LastName);
    end);

  TelegramClient1.Methods.Execute(TGetMe.Create, '',
    procedure(User: TJSONObject)
    begin
      Memo1.Lines.Add('TelegramClient1.Methods.GetMe callback'#13#10 + User.Format);
    end);
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  TelegramClient1.Methods.SendMessage(
    TBuildSendMessage.Create.InputMessageContent(
      TInputMessageText.Create.Text(
          TFormattedText.Create.Text('Hello').Entities(
           [TTextEntity.Create.Offset(0).Length(2).EntityType(TTextEntityTypePreCode.Create('pascal'))]
           )
        )
    ).ChatId(1288857534), //268284944
    procedure(Msg: TtgMessage)
    begin
      // сообщение отправлено
      Memo1.Lines.Add('sended msg');
    end);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  //if TelegramClient1.IsInitialized then
  //  ShowMessage('ok');
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  TThread.RemoveQueuedEvents(nil);
end;

procedure TForm4.TelegramClient1AuthReady(Sender: TObject);
begin
  ShowMessage('ready');
end;

procedure TForm4.TelegramClient1Close(Sender: TObject);
begin
  Memo1.Lines.Add('Client closed. Recreating');
  TelegramClient1.Recreate;
end;

procedure TForm4.TelegramClient1Error(Sender: TObject; const Code: Integer; const Message: string);
begin
  if Code <> 406 then
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

