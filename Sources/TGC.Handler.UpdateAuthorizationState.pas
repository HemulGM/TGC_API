unit TGC.Handler.UpdateAuthorizationState;

interface

uses
  System.Classes, System.SysUtils, System.JSON, TGC.Handler, HGM.JSONParams;

type
  TSetTdlibParameters = class(TJSONParam)
  end;

  TTermsOfServiceText = class
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TTermsOfService = class
  private
    FMin_user_age: Integer;
    FShow_popup: Boolean;
    FText: TTermsOfServiceText;
  public
    property MinUserAge: Integer read FMin_user_age write FMin_user_age;
    property ShowPopup: Boolean read FShow_popup write FShow_popup;
    property Text: TTermsOfServiceText read FText write FText;
    destructor Destroy; override;
  end;

  TUpdateAuthorizationState = class(THandler)
  private
    procedure SendSetParams;
    procedure DoNeedRegistration(JSON: TJSONObject);
  public
    procedure Execute(JSON: TJSONObject); override;
  end;

implementation

uses
  TGC.Client, REST.Json;

{ TUpdateAuthorizationState }

procedure TUpdateAuthorizationState.Execute(JSON: TJSONObject);
begin
  inherited;
  var JO: TJSONObject;
  if JSON.TryGetValue('authorization_state', JO) then
  begin
    var AType := JO.GetValue('@type', '');
    if AType = 'authorizationStateWaitTdlibParameters' then
      SendSetParams
    else if AType = 'authorizationStateClosed' then
      TTelegramClientCustom(Client).Close
    else if AType = 'authorizationStateWaitEncryptionKey' then
      with TTelegramClientCustom(Client) do
        Send('checkDatabaseEncryptionKey', 'encryption_key', TJSONString.Create(Parameters.DatabaseEncryptionKey))
    else if AType = 'authorizationStateWaitPhoneNumber' then
      with TTelegramClientCustom(Client) do
        Send('setAuthenticationPhoneNumber', 'phone_number', TJSONString.Create(PhoneNumber))
    else if AType = 'authorizationStateWaitCode' then
      with TTelegramClientCustom(Client) do
        NeedAuthCode
    else if AType = 'authorizationStateWaitRegistration' then
      DoNeedRegistration(JO.GetValue<TJSONObject>('terms_of_service', nil))
    else if AType = 'authorizationStateWaitPassword' then
      with TTelegramClientCustom(Client) do
        NeedAuthenticationPassword
    else if AType = 'authorizationStateWaitOtherDeviceConfirmation' then
      with TTelegramClientCustom(Client) do
        NeedOtherDeviceConfirmation(JO.GetValue('link', ''))
    else if AType = 'authorizationStateReady' then
      with TTelegramClientCustom(Client) do
        AuthReady;
  end;
end;

procedure TUpdateAuthorizationState.DoNeedRegistration(JSON: TJSONObject);
var
  Terms: TTermsOfService;
begin
  try
    Terms := TJson.JsonToObject<TTermsOfService>(JSON);
  except
    Terms := nil;
  end;
  with TTelegramClientCustom(Client) do
    NeedRegistration(Terms);
end;

procedure TUpdateAuthorizationState.SendSetParams;
var
  Params: TSetTdlibParameters;
begin
  with TTelegramClientCustom(Client) do
  begin
    Params := TSetTdlibParameters.Create;
    try
      if UseTestDC then
        Params.Add('use_test_dc', True);
      Params.Add('database_directory', Parameters.DatabaseDirectory);
      Params.Add('files_directory', Parameters.FilesDirectory);
      Params.Add('use_file_database', Parameters.UseFileDatabase);
      Params.Add('use_chat_info_database', Parameters.UseChatInfoDatabase);
      Params.Add('use_message_database', Parameters.UseMessageDatabase);
      Params.Add('use_secret_chats', Parameters.UseSecretChats);
      Params.Add('api_id', ApiId);
      Params.Add('api_hash', ApiHash);
      if not BotToken.IsEmpty then
        Params.Add('token', BotToken);
      Params.Add('system_language_code', Parameters.SystemLanguageCode);
      Params.Add('device_model', Parameters.DeviceModel);
      Params.Add('system_version', Parameters.SystemVersion);
      Params.Add('application_version', Parameters.ApplicationVersion);
      Params.Add('enable_storage_optimizer', Parameters.EnableStorageOptimizer);
      Params.Add('ignore_file_names', Parameters.IgnoreFileNames);
      Send('setTdlibParameters', '', Params.Clone);
    finally
      Params.Free;
    end;
  end;
end;

{ TTermsOfService }

destructor TTermsOfService.Destroy;
begin
  if Assigned(FText) then
    FText.Free;
  inherited;
end;

end.

