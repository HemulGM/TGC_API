unit TGC.Component;

interface

uses
  System.SysUtils, System.Classes, TGC.Client;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Telegram Client', [TTelegramClient]);
end;

end.

