program TGClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  TGC.Component in '..\..\Package\TGC.Component.pas',
  HGM.JSONParams in '..\..\Sources\HGM.JSONParams.pas',
  TGC.Client in '..\..\Sources\TGC.Client.pas',
  TGC.Handler.Error in '..\..\Sources\TGC.Handler.Error.pas',
  TGC.Handler in '..\..\Sources\TGC.Handler.pas',
  TGC.Handler.UpdateAuthorizationState in '..\..\Sources\TGC.Handler.UpdateAuthorizationState.pas',
  TGC.Wrapper in '..\..\Sources\TGC.Wrapper.pas',
  TGC.Errors in '..\..\Sources\TGC.Errors.pas',
  TGC.Entity.User in '..\..\Sources\TGC.Entity.User.pas',
  TGC.Classes in '..\..\Sources\TGC.Classes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
