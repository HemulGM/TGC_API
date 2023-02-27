unit TGC.Handler.Error;

interface

uses
  System.Classes, System.SysUtils, System.JSON, TGC.Handler;

type
  TError = class(THandler)
  public
    procedure Execute(JSON: TJSONObject); override;
  end;

implementation

uses
  TGC.Client;

{ TError }

procedure TError.Execute(JSON: TJSONObject);
begin
  inherited;
  TTelegramClientCustom(Client).Error(JSON.GetValue<integer>('code', -1), JSON.GetValue('message', ''));
end;

end.

