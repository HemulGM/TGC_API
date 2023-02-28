program TGClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  TGC.Component in '..\..\Package\TGC.Component.pas',
  HGM.JSONParams in '..\..\Sources\HGM.JSONParams.pas',
  TGC.Classes in '..\..\Sources\TGC.Classes.pas',
  TGC.Client in '..\..\Sources\TGC.Client.pas',
  TGC.Entity.User in '..\..\Sources\TGC.Entity.User.pas',
  TGC.Errors in '..\..\Sources\TGC.Errors.pas',
  TGC.Handler.Error in '..\..\Sources\TGC.Handler.Error.pas',
  TGC.Handler in '..\..\Sources\TGC.Handler.pas',
  TGC.Handler.UpdateAuthorizationState in '..\..\Sources\TGC.Handler.UpdateAuthorizationState.pas',
  TGC.Handler.UpdateOption in '..\..\Sources\TGC.Handler.UpdateOption.pas',
  TGC.Options in '..\..\Sources\TGC.Options.pas',
  TGC.Wrapper in '..\..\Sources\TGC.Wrapper.pas',
  TGC.Entity.ProfilePhoto in '..\..\Sources\TGC.Entity.ProfilePhoto.pas',
  TGC.Entity.Files in '..\..\Sources\TGC.Entity.Files.pas',
  TGC.Entity.Message in '..\..\Sources\TGC.Entity.Message.pas',
  TGC.Entity.Sticker in '..\..\Sources\TGC.Entity.Sticker.pas',
  TGC.Builder.SendMessage in '..\..\Sources\TGC.Builder.SendMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
