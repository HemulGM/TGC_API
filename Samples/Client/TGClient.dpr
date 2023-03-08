program TGClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  TGC.Component in '..\..\Package\TGC.Component.pas',
  HGM.JSONParams in '..\..\Sources\HGM.JSONParams.pas',
  TGC.Classes in '..\..\Sources\TGC.Classes.pas',
  TGC.Client in '..\..\Sources\TGC.Client.pas',
  TGC.Errors in '..\..\Sources\TGC.Errors.pas',
  TGC.Handler.Error in '..\..\Sources\TGC.Handler.Error.pas',
  TGC.Handler in '..\..\Sources\TGC.Handler.pas',
  TGC.Handler.UpdateAuthorizationState in '..\..\Sources\TGC.Handler.UpdateAuthorizationState.pas',
  TGC.Handler.UpdateOption in '..\..\Sources\TGC.Handler.UpdateOption.pas',
  TGC.Options in '..\..\Sources\TGC.Options.pas',
  TGC.Wrapper in '..\..\Sources\TGC.Wrapper.pas',
  TGC.Builder.SendMessage in '..\..\Sources\TGC.Builder.SendMessage.pas',
  TGC.Builder.GetMe in '..\..\Sources\TGC.Builder.GetMe.pas',
  TGC.Builder.SendMessageAlbum in '..\..\Sources\TGC.Builder.SendMessageAlbum.pas',
  TGC.Builder.GetUser in '..\..\Sources\TGC.Builder.GetUser.pas',
  TGC.Builder.GetUserFullInfo in '..\..\Sources\TGC.Builder.GetUserFullInfo.pas',
  TGC.Entity.AnimatedChatPhoto in '..\..\Sources\TGC.Entity.AnimatedChatPhoto.pas',
  TGC.Entity.AObject in '..\..\Sources\TGC.Entity.AObject.pas',
  TGC.Entity.BotCommand in '..\..\Sources\TGC.Entity.BotCommand.pas',
  TGC.Entity.ChatPhoto in '..\..\Sources\TGC.Entity.ChatPhoto.pas',
  TGC.Entity.Files in '..\..\Sources\TGC.Entity.Files.pas',
  TGC.Entity.FormatedText in '..\..\Sources\TGC.Entity.FormatedText.pas',
  TGC.Entity.Message in '..\..\Sources\TGC.Entity.Message.pas',
  TGC.Entity.MiniThumbnail in '..\..\Sources\TGC.Entity.MiniThumbnail.pas',
  TGC.Entity.PhotoSize in '..\..\Sources\TGC.Entity.PhotoSize.pas',
  TGC.Entity.ProfilePhoto in '..\..\Sources\TGC.Entity.ProfilePhoto.pas',
  TGC.Entity.Sticker in '..\..\Sources\TGC.Entity.Sticker.pas',
  TGC.Entity.User in '..\..\Sources\TGC.Entity.User.pas',
  TGC.Entity.UserFullInfo in '..\..\Sources\TGC.Entity.UserFullInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
