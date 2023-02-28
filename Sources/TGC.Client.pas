unit TGC.Client;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Threading,
  System.JSON, TGC.Wrapper, TGC.Handler, TGC.Handler.UpdateAuthorizationState,
  TGC.Entity.User, TGC.Classes, TGC.Options, TGC.Builder.SendMessage,
  TGC.Entity.Message;

const
  DEFAULT_WAIT_TIMEOUT = 10.0;

type
  TRequestId = Int64;

  TTelegramClientCustom = class;

  TOnReceiveRaw = procedure(Sender: TObject; const Data: string) of object;

  TOnNeedAuthConfirm = procedure(Sender: TObject; const Link: string) of object;

  TOnNeedRegistration = procedure(Sender: TObject; const Terms: TTermsOfService) of object;

  TOnError = procedure(Sender: TObject; const Code: Integer; const Message: string) of object;

  THandlers = TObjectDictionary<string, THandler>;

  TTDLibParameters = class(TPersistent)
  private
    FUseFileDatabase: Boolean;
    FDatabaseDirectory: string;
    FUseChatInfoDatabase: Boolean;
    FFilesDirectory: string;
    FUseMessageDatabase: Boolean;
    FUseSecretChats: Boolean;
    FEnableStorageOptimizer: Boolean;
    FIgnoreFileNames: Boolean;
    FSystemVersion: string;
    FDeviceModel: string;
    FSystemLanguageCode: string;
    FApplicationVersion: string;
    FDatabaseEncryptionKey: string;
    procedure SetDatabaseDirectory(const Value: string);
    procedure SetFilesDirectory(const Value: string);
    procedure SetUseChatInfoDatabase(const Value: Boolean);
    procedure SetUseFileDatabase(const Value: Boolean);
    procedure SetUseMessageDatabase(const Value: Boolean);
    procedure SetUseSecretChats(const Value: Boolean);
    procedure SetApplicationVersion(const Value: string);
    procedure SetDeviceModel(const Value: string);
    procedure SetEnableStorageOptimizer(const Value: Boolean);
    procedure SetIgnoreFileNames(const Value: Boolean);
    procedure SetSystemLanguageCode(const Value: string);
    procedure SetSystemVersion(const Value: string);
    procedure SetDatabaseEncryptionKey(const Value: string);
  published
    /// <summary>
    /// The path to the directory for the persistent database; if empty, the current working directory will be used.
    /// </summary>
    property DatabaseDirectory: string read FDatabaseDirectory write SetDatabaseDirectory;
    /// <summary>
    /// The path to the directory for storing files; if empty, DatabaseDirectory will be used.
    /// </summary>
    property FilesDirectory: string read FFilesDirectory write SetFilesDirectory;
    /// <summary>
    /// If set to true, information about downloaded and uploaded files will be saved between application restarts.
    /// </summary>
    property UseFileDatabase: Boolean read FUseFileDatabase write SetUseFileDatabase default True;
    /// <summary>
    /// If set to true, the library will maintain a cache of users, basic groups, supergroups, channels and secret chats. Implies UseFileDatabase.
    /// </summary>
    property UseChatInfoDatabase: Boolean read FUseChatInfoDatabase write SetUseChatInfoDatabase default True;
    /// <summary>
    /// If set to true, the library will maintain a cache of chats and messages. Implies UseChatInfoDatabase.
    /// </summary>
    property UseMessageDatabase: Boolean read FUseMessageDatabase write SetUseMessageDatabase default True;
    /// <summary>
    /// If set to true, support for secret chats will be enabled.
    /// </summary>
    property UseSecretChats: Boolean read FUseSecretChats write SetUseSecretChats default True;
    /// <summary>
    /// IETF language tag of the user's operating system language; must be non-empty.
    /// </summary>
    property SystemLanguageCode: string read FSystemLanguageCode write SetSystemLanguageCode;
    /// <summary>
    /// Model of the device the application is being run on; must be non-empty.
    /// </summary>
    property DeviceModel: string read FDeviceModel write SetDeviceModel;
    /// <summary>
    /// Version of the operating system the application is being run on. If empty, the version is automatically detected by TDLib.
    /// </summary>
    property SystemVersion: string read FSystemVersion write SetSystemVersion;
    /// <summary>
    /// Application version; must be non-empty.
    /// </summary>
    property ApplicationVersion: string read FApplicationVersion write SetApplicationVersion;
    /// <summary>
    /// If set to true, old files will automatically be deleted.
    /// </summary>
    property EnableStorageOptimizer: Boolean read FEnableStorageOptimizer write SetEnableStorageOptimizer default False;
    /// <summary>
    /// If set to true, original file names will be ignored. Otherwise, downloaded files will be saved under names as close as possible to the original name.
    /// </summary>
    property IgnoreFileNames: Boolean read FIgnoreFileNames write SetIgnoreFileNames default False;
    /// <summary>
    /// Encryption key for database to let know TDLib how to open the database
    /// </summary>
    property DatabaseEncryptionKey: string read FDatabaseEncryptionKey write SetDatabaseEncryptionKey;
  end;

  TDLibMethods = class
  private
    FPoll: TThreadDict<TRequestId, TProc<TJSONObject>>;
    FClient: TTelegramClientCustom;
    FRequestQueue: TRequestId;
    FSync: Boolean;
    function NewRequestId: TRequestId;
    procedure SetSync(const Value: Boolean);
    procedure Sync(Proc: TProc);
  protected
    procedure Clear;
    function Proc(const RequestId: TRequestId; JSON: TJSONObject): Boolean;
    property SyncCallback: Boolean read FSync write SetSync;
  public
    procedure GetMe(Callback: TProc<TtgUser>);
    procedure SendMessage(Params: TBuildSendMessage; Callback: TProc<TtgMessage>);
    procedure Execute<T: class, constructor>(Query: TParam; FieldName: string; Callback: TProc<T>); overload;
    procedure Execute(Query: TParam; FieldName: string; Callback: TProc<TJSONObject>); overload;
    constructor Create(Client: TTelegramClientCustom);
    destructor Destroy; override;
  end;

  TTelegramClientCustom = class(TComponent)
  private
    FClient: TVoid;
    FReceiver: ITask;
    FOnReceive: TOnReceiveRaw;
    FDoStopReceiver: Boolean;
    FSyncEvents: Boolean;
    FTimeout: Double;
    FHandlers: THandlers;
    FUseTestDC: Boolean;
    FParameters: TTDLibParameters;
    FApiHash: string;
    FApiId: Int32;
    FBotToken: string;
    FPhoneNumber: string;
    FOnNeedAuthCode: TNotifyEvent;
    FOnRegistration: TOnNeedRegistration;
    FOnNeedAuthPassword: TNotifyEvent;
    FOnNeedAuthConfirm: TOnNeedAuthConfirm;
    FOnAuthReady: TNotifyEvent;
    FOnError: TOnError;
    FMethods: TDLibMethods;
    FOnClose: TNotifyEvent;
    FOptions: TtgOptions;
    function GetIsInitialized: Boolean;
    procedure SetOnReceive(const Value: TOnReceiveRaw);
    procedure StartReceiver;
    procedure ReceiverWorker;
    procedure SetSyncEvents(const Value: Boolean);
    procedure SetTimeout(const Value: Double);
    procedure ProcReceive(JSON: TJSONObject);
    procedure DoReceiveRaw(const JSON: string);
    procedure CreateHandlers;
    procedure SetUseTestDC(const Value: Boolean);
    procedure SetApiHash(const Value: string);
    procedure SetApiId(const Value: Int32);
    procedure DefaultParameters;
    function GetInternalSystemLangCode: string;
    procedure SetBotToken(const Value: string);
    procedure SetPhoneNumber(const Value: string);
    procedure SetOnNeedAuthCode(const Value: TNotifyEvent);
    procedure SetOnRegistration(const Value: TOnNeedRegistration);
    procedure SetOnNeedAuthPassword(const Value: TNotifyEvent);
    procedure SetOnNeedAuthConfirm(const Value: TOnNeedAuthConfirm);
    procedure SetOnAuthReady(const Value: TNotifyEvent);
    procedure SetOnError(const Value: TOnError);
    procedure InternalRecreate;
    procedure DoClose;
    procedure SetOnClose(const Value: TNotifyEvent);
    procedure SetSyncMethodsCallback(const Value: Boolean);
    function GetSyncMethodsCallback: Boolean;
    procedure Sync(Proc: TProc);
  protected
    procedure InitializateLib;
    procedure InternalClose;
  public
    // service
    procedure Close;
    procedure NeedAuthCode;
    procedure NeedRegistration(Terms: TTermsOfService);
    procedure NeedAuthenticationPassword;
    procedure NeedOtherDeviceConfirmation(const Link: string);
    procedure AuthReady;
    procedure Send(const JSON: TJSONObject; AutoFree: Boolean = True); overload;
    procedure Send(const AType, AName: string; const JSON: TJSONValue); overload;
    procedure Send(const AType: string; APairs: TArray<TPair<string, TJSONValue>>); overload;
    function Receive(const TimeOut: Double): TJSONObject;
    function Execute(const JSON: TJSONObject; AutoFree: Boolean = True): TJSONObject;
    procedure Error(const Code: Integer; const Message: string);
    // user
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Recreate;
    property Client: TVoid read FClient;
    property IsInitialized: Boolean read GetIsInitialized;
    procedure SetAuthCode(const Value: string);
    procedure SetRegisterUser(const FirstName, LastName: string);
    procedure SetAuthPassword(const Value: string);
    /// <summary>
    /// Accessing API Methods
    /// </summary>
    property Methods: TDLibMethods read FMethods;
    /// <summary>
    /// List of session options
    /// </summary>
    property Options: TtgOptions read FOptions;
  published
    property OnReceive: TOnReceiveRaw read FOnReceive write SetOnReceive;
    property SyncEvents: Boolean read FSyncEvents write SetSyncEvents;
    property SyncMethodsCallback: Boolean read GetSyncMethodsCallback write SetSyncMethodsCallback;
    property Timeout: Double read FTimeout write SetTimeout;
    property UseTestDC: Boolean read FUseTestDC write SetUseTestDC;
    property Parameters: TTDLibParameters read FParameters write FParameters;
    property ApiId: Int32 read FApiId write SetApiId;
    property ApiHash: string read FApiHash write SetApiHash;
    property BotToken: string read FBotToken write SetBotToken;
    property PhoneNumber: string read FPhoneNumber write SetPhoneNumber;
    property OnNeedAuthCode: TNotifyEvent read FOnNeedAuthCode write SetOnNeedAuthCode;
    property OnRegistration: TOnNeedRegistration read FOnRegistration write SetOnRegistration;
    property OnNeedAuthPassword: TNotifyEvent read FOnNeedAuthPassword write SetOnNeedAuthPassword;
    property OnNeedAuthConfirm: TOnNeedAuthConfirm read FOnNeedAuthConfirm write SetOnNeedAuthConfirm;
    property OnAuthReady: TNotifyEvent read FOnAuthReady write SetOnAuthReady;
    property OnClose: TNotifyEvent read FOnClose write SetOnClose;
    property OnError: TOnError read FOnError write SetOnError;
  end;

  TTelegramClient = class(TTelegramClientCustom)
  published
    /// <summary>
    /// Application identifier hash for Telegram API access, which can be obtained at https://my.telegram.org.
    /// </summary>
    property ApiHash;
    /// <summary>
    /// Application identifier for Telegram API access, which can be obtained at https://my.telegram.org.
    /// </summary>
    property ApiId default 0;
    /// <summary>
    /// Bot token for use client as bot
    /// </summary>
    property BotToken;
    /// <summary>
    /// The user has been successfully authorized. TDLib is now ready to answer queries.
    /// </summary>
    property OnAuthReady;
    /// <summary>
    /// TDLib client is in its final state. All databases are closed and all resources are released.
    /// No other updates will be received after this. All queries will be responded to with error code 500.
    /// To continue working, one must create a new instance of the TDLib client.
    /// </summary>
    property OnClose;
    /// <summary>
    /// Error handle
    /// </summary>
    property OnError;
    /// <summary>
    /// Authorization code needed. Use SetAuthCode
    /// </summary>
    property OnNeedAuthCode;
    /// <summary>
    /// The user needs to confirm authorization on another logged in device by scanning a QR code
    /// with the provided link
    /// </summary>
    property OnNeedAuthConfirm;
    /// <summary>
    /// The user has been authorized, but needs to enter a password to start using the application.
    /// Use SetAuthPassword
    /// </summary>
    property OnNeedAuthPassword;
    /// <summary>
    /// TDLib receive raw data
    /// </summary>
    property OnReceive;
    /// <summary>
    /// Finishes user registration. Use SetRegisterUser
    /// </summary>
    property OnRegistration;
    /// <summary>
    /// Contains parameters for TDLib initialization.
    /// </summary>
    property Parameters;
    /// <summary>
    /// User phone number
    /// </summary>
    property PhoneNumber;
    /// <summary>
    /// Do synchronize component events
    /// </summary>
    property SyncEvents default False;
    /// <summary>
    /// Do synchronize methods callback
    /// </summary>
    property SyncMethodsCallback default False;
    /// <summary>
    /// Receive timeout (in seconds). Default 10.0 sec
    /// </summary>
    property Timeout;
    /// <summary>
    /// If set to true, the Telegram test environment will be used instead of the production environment.
    /// </summary>
    property UseTestDC default False;
  end;

implementation

uses
  TGC.Handler.Error, REST.Json, TGC.Handler.UpdateOption;

{ TTelegramClientCustom }

procedure TTelegramClientCustom.InternalClose;
begin
  FMethods.Clear;
  if FReceiver <> nil then
  begin
    FDoStopReceiver := True;
    FReceiver.Wait;
    FReceiver := nil;
  end;
  if FClient <> 0 then
  begin
    JsonClientDestroy(FClient);
    FClient := 0;
  end;
end;

procedure TTelegramClientCustom.Close;
begin
  InternalClose;
  DoClose;
end;

constructor TTelegramClientCustom.Create;
begin
  inherited;
  FOptions := TtgOptions.Create;
  FMethods := TDLibMethods.Create(Self);
  FHandlers := THandlers.Create([doOwnsValues]);
  FParameters := TTDLibParameters.Create;
  DefaultParameters;
  CreateHandlers;
  FClient := 0;
  FReceiver := nil;
  FTimeout := DEFAULT_WAIT_TIMEOUT;
  FSyncEvents := False;
  FDoStopReceiver := False;
  if not (csDesigning in ComponentState) then
    InitializateLib;
end;

destructor TTelegramClientCustom.Destroy;
begin
  InternalClose;
  FMethods.Free;
  FHandlers.Free;
  FParameters.Free;
  FOptions.Free;
  TDLibFinalize;
  inherited;
end;

procedure TTelegramClientCustom.Sync(Proc: TProc);
begin
  TThread.Queue(nil,
    procedure
    begin
      Proc;
    end);
end;

procedure TTelegramClientCustom.Error(const Code: Integer; const Message: string);
begin
  if Assigned(FOnError) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          FOnError(Self, Code, Message);
        end)
    else
      FOnError(Self, Code, Message);
end;

function TTelegramClientCustom.Execute(const JSON: TJSONObject; AutoFree: Boolean): TJSONObject;
var
  JSONString: string;
begin
  Result := nil;
  try
    JSONString := JSON.ToJSON;
  finally
    if AutoFree then
      JSON.Free;
  end;
  JSONString := TgCharToString(JsonClientExecute(FClient, StringToTgChar(JSONString)));
  if not JSONString.IsEmpty then
    Result := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
end;

function TTelegramClientCustom.GetIsInitialized: Boolean;
begin
  Result := FClient <> 0;
end;

function TTelegramClientCustom.GetSyncMethodsCallback: Boolean;
begin
  Result := FMethods.SyncCallback;
end;

function TTelegramClientCustom.GetInternalSystemLangCode: string;
var
  ID: Integer;
begin
  ID := Languages.IndexOf(Languages.UserDefaultLocale);
  if ID >= 0 then
    Result := Languages.LocaleName[ID].SubString(0, 2)
  else
    Result := '';
end;

procedure TTelegramClientCustom.DefaultParameters;
begin
  FParameters.DatabaseDirectory := 'tdlib';
  FParameters.FilesDirectory := 'tdlib_files';
  FParameters.UseFileDatabase := True;
  FParameters.UseChatInfoDatabase := True;
  FParameters.UseMessageDatabase := True;
  FParameters.UseSecretChats := True;
  FParameters.SystemLanguageCode := GetInternalSystemLangCode;
  FParameters.ApplicationVersion := '0.1';
  FParameters.DeviceModel := 'Desktop';
end;

procedure TTelegramClientCustom.CreateHandlers;
begin
  FHandlers.Clear;
  FHandlers.Add('error', TError.Create(Self));
  FHandlers.Add('updateAuthorizationState', TUpdateAuthorizationState.Create(Self));
  FHandlers.Add('updateOption', TUpdateOption.Create(Self));
end;

procedure TTelegramClientCustom.Recreate;
begin
  InternalClose;
  InternalRecreate;
end;

procedure TTelegramClientCustom.InternalRecreate;
begin
  FClient := JsonCreateClient;
  StartReceiver;
end;

procedure TTelegramClientCustom.InitializateLib;
begin
  if TDLibInitialize then
    InternalRecreate;
end;

procedure TTelegramClientCustom.AuthReady;
begin
  if Assigned(FOnAuthReady) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          FOnAuthReady(Self);
        end)
    else
      FOnAuthReady(Self);
end;

procedure TTelegramClientCustom.DoClose;
begin
  if Assigned(FOnClose) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          FOnClose(Self);
        end)
    else
      FOnClose(Self);
end;

procedure TTelegramClientCustom.NeedAuthCode;
begin
  if Assigned(FOnNeedAuthCode) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          FOnNeedAuthCode(Self);
        end)
    else
      FOnNeedAuthCode(Self);
end;

procedure TTelegramClientCustom.NeedAuthenticationPassword;
begin
  if Assigned(FOnNeedAuthPassword) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          FOnNeedAuthPassword(Self);
        end)
    else
      FOnNeedAuthPassword(Self);
end;

procedure TTelegramClientCustom.NeedOtherDeviceConfirmation(const Link: string);
begin
  if Assigned(FOnNeedAuthConfirm) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          FOnNeedAuthConfirm(Self, Link);
        end)
    else
      FOnNeedAuthConfirm(Self, Link);
end;

procedure TTelegramClientCustom.NeedRegistration(Terms: TTermsOfService);
begin
  if Assigned(FOnRegistration) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          try
            FOnRegistration(Self, Terms);
          finally
            Terms.Free;
          end;
        end)
    else
    try
      FOnRegistration(Self, Terms);
    finally
      Terms.Free;
    end
  else
    Terms.Free;
end;

procedure TTelegramClientCustom.DoReceiveRaw(const JSON: string);
begin
  if Assigned(FOnReceive) then
    if FSyncEvents then
      Sync(
        procedure
        begin
          FOnReceive(Self, JSON);
        end)
    else
      FOnReceive(Self, JSON);
end;

procedure TTelegramClientCustom.StartReceiver;
begin
  FReceiver := TTask.Run(ReceiverWorker);
end;

procedure TTelegramClientCustom.ReceiverWorker;
begin
  while not ((FDoStopReceiver) or (TTask.CurrentTask.Status = TTaskStatus.Canceled)) do
  try
    ProcReceive(Receive(FTimeout));
  except
    on E: Exception do
      Error(-1, E.Message);
  end;
end;

function TTelegramClientCustom.Receive(const TimeOut: Double): TJSONObject;
var
  JSONString: string;
begin
  Result := nil;
  JSONString := TgCharToString(JsonClientReceive(FClient, TimeOut));
  if not JSONString.IsEmpty then
    Result := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
end;

procedure TTelegramClientCustom.ProcReceive(JSON: TJSONObject);
var
  AExtra: TRequestId;
  AHandler: THandler;
begin
  if Assigned(JSON) then
  try
    // Обработка запросов с указанным RequestId
    AExtra := JSON.GetValue<TRequestId>('@extra', -1);
    if AExtra >= 0 then
      if FMethods.Proc(AExtra, JSON) then
        Exit;

    // Обработка запросов подписанными обработчиками
    if FHandlers.TryGetValue(JSON.GetValue('@type', ''), AHandler) then
    begin
      AHandler.Execute(JSON);
      Exit;
    end;

    // Событие с сырыми данными для пользовательской обработки
    DoReceiveRaw(JSON.ToJSON);
  finally
    JSON.Free;
  end;
end;

procedure TTelegramClientCustom.Send(const JSON: TJSONObject; AutoFree: Boolean);
var
  JSONString: string;
begin
  try
    JSONString := JSON.ToJSON;
  finally
    if AutoFree then
      JSON.Free;
  end;
  DoReceiveRaw(JSONString);
  JsonClientSend(FClient, StringToTgChar(JSONString));
end;

procedure TTelegramClientCustom.Send(const AType, AName: string; const JSON: TJSONValue);
var
  JSONObject: TJSONObject;
begin
  if Assigned(JSON) and AName.IsEmpty and (JSON is TJSONObject) then
    JSONObject := JSON as TJSONObject
  else
  begin
    JSONObject := TJSONObject.Create;
    if Assigned(JSON) then
      JSONObject.AddPair(AName, JSON);
  end;
  JSONObject.AddPair('@type', AType);
  Send(JSONObject);
end;

procedure TTelegramClientCustom.Send(const AType: string; APairs: TArray<TPair<string, TJSONValue>>);
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  JSONObject.AddPair('@type', AType);
  for var Pair in APairs do
    JSONObject.AddPair(Pair.Key, Pair.Value);
  Send(JSONObject);
end;

procedure TTelegramClientCustom.SetApiHash(const Value: string);
begin
  FApiHash := Value;
end;

procedure TTelegramClientCustom.SetApiId(const Value: Int32);
begin
  FApiId := Value;
end;

procedure TTelegramClientCustom.SetAuthCode(const Value: string);
begin
  Send('checkAuthenticationCode', 'code', TJSONString.Create(Value));
end;

procedure TTelegramClientCustom.SetAuthPassword(const Value: string);
begin
  Send('checkAuthenticationPassword', 'password', TJSONString.Create(Value));
end;

procedure TTelegramClientCustom.SetBotToken(const Value: string);
begin
  FBotToken := Value;
end;

procedure TTelegramClientCustom.SetOnAuthReady(const Value: TNotifyEvent);
begin
  FOnAuthReady := Value;
end;

procedure TTelegramClientCustom.SetOnClose(const Value: TNotifyEvent);
begin
  FOnClose := Value;
end;

procedure TTelegramClientCustom.SetOnError(const Value: TOnError);
begin
  FOnError := Value;
end;

procedure TTelegramClientCustom.SetOnNeedAuthCode(const Value: TNotifyEvent);
begin
  FOnNeedAuthCode := Value;
end;

procedure TTelegramClientCustom.SetOnNeedAuthConfirm(const Value: TOnNeedAuthConfirm);
begin
  FOnNeedAuthConfirm := Value;
end;

procedure TTelegramClientCustom.SetOnNeedAuthPassword(const Value: TNotifyEvent);
begin
  FOnNeedAuthPassword := Value;
end;

procedure TTelegramClientCustom.SetOnReceive(const Value: TOnReceiveRaw);
begin
  FOnReceive := Value;
end;

procedure TTelegramClientCustom.SetOnRegistration(const Value: TOnNeedRegistration);
begin
  FOnRegistration := Value;
end;

procedure TTelegramClientCustom.SetPhoneNumber(const Value: string);
begin
  FPhoneNumber := Value;
end;

procedure TTelegramClientCustom.SetRegisterUser(const FirstName, LastName: string);
begin
  Send('registerUser', [
    TPair<string, TJSONValue>.Create('first_name', TJSONString.Create(FirstName)),
    TPair<string, TJSONValue>.Create('last_name', TJSONString.Create(LastName))
    ]);
end;

procedure TTelegramClientCustom.SetSyncEvents(const Value: Boolean);
begin
  FSyncEvents := Value;
end;

procedure TTelegramClientCustom.SetSyncMethodsCallback(const Value: Boolean);
begin
  FMethods.SyncCallback := Value;
end;

procedure TTelegramClientCustom.SetTimeout(const Value: Double);
begin
  FTimeout := Value;
end;

procedure TTelegramClientCustom.SetUseTestDC(const Value: Boolean);
begin
  FUseTestDC := Value;
end;

{ TTDLibParameters }

procedure TTDLibParameters.SetApplicationVersion(const Value: string);
begin
  FApplicationVersion := Value;
end;

procedure TTDLibParameters.SetDatabaseDirectory(const Value: string);
begin
  FDatabaseDirectory := Value;
end;

procedure TTDLibParameters.SetDatabaseEncryptionKey(const Value: string);
begin
  FDatabaseEncryptionKey := Value;
end;

procedure TTDLibParameters.SetDeviceModel(const Value: string);
begin
  FDeviceModel := Value;
end;

procedure TTDLibParameters.SetEnableStorageOptimizer(const Value: Boolean);
begin
  FEnableStorageOptimizer := Value;
end;

procedure TTDLibParameters.SetFilesDirectory(const Value: string);
begin
  FFilesDirectory := Value;
end;

procedure TTDLibParameters.SetIgnoreFileNames(const Value: Boolean);
begin
  FIgnoreFileNames := Value;
end;

procedure TTDLibParameters.SetSystemLanguageCode(const Value: string);
begin
  FSystemLanguageCode := Value;
end;

procedure TTDLibParameters.SetSystemVersion(const Value: string);
begin
  FSystemVersion := Value;
end;

procedure TTDLibParameters.SetUseChatInfoDatabase(const Value: Boolean);
begin
  FUseChatInfoDatabase := Value;
end;

procedure TTDLibParameters.SetUseFileDatabase(const Value: Boolean);
begin
  FUseFileDatabase := Value;
end;

procedure TTDLibParameters.SetUseMessageDatabase(const Value: Boolean);
begin
  FUseMessageDatabase := Value;
end;

procedure TTDLibParameters.SetUseSecretChats(const Value: Boolean);
begin
  FUseSecretChats := Value;
end;

{ TDLibMethods }

procedure TDLibMethods.Clear;
begin
  FPoll.Clear;
end;

constructor TDLibMethods.Create(Client: TTelegramClientCustom);
begin
  inherited Create;
  FSync := False;
  FClient := Client;
  FPoll := TThreadDict<TRequestId, TProc<TJSONObject>>.Create;
  FRequestQueue := 0;
end;

destructor TDLibMethods.Destroy;
begin
  FPoll.Free;
  inherited;
end;

procedure TDLibMethods.Execute(Query: TParam; FieldName: string; Callback: TProc<TJSONObject>);
begin
  try
    var RequestId := NewRequestId;
    Query.Extra(RequestId);
    if Assigned(Callback) then
      FPoll.Add(RequestId,
        procedure(JSON: TJSONObject)
        begin
          if not FSync then
            Callback(JSON)
          else
          begin
            var JO := TJSONObject(JSON.Clone);
            Sync(
              procedure
              begin
                try
                  Callback(JO);
                finally
                  JO.Free;
                end;
              end);
          end;
        end)
    else
      FPoll.Add(RequestId, nil);
    FClient.Send(Query.JSON, False);
  finally
    Query.Free;
  end;
end;

procedure TDLibMethods.Execute<T>(Query: TParam; FieldName: string; Callback: TProc<T>);
begin
  try
    var RequestId := NewRequestId;
    Query.Extra(RequestId);
    if Assigned(Callback) then
      FPoll.Add(RequestId,
        procedure(JSON: TJSONObject)
        var
          JO: TJSONObject;
          Obj: T;
        begin
          if FieldName.IsEmpty then
            JO := JSON
          else
            JO := JSON.GetValue<TJSONObject>(FieldName, nil);
          if Assigned(JO) then
          begin
            Obj := TJson.JsonToObject<T>(JO);
            if not FSync then
            try
              Callback(Obj);
            finally
              Obj.Free;
            end
            else
              Sync(
                procedure
                begin
                  try
                    Callback(Obj);
                  finally
                    Obj.Free;
                  end;
                end);
          end;
        end)
    else
      FPoll.Add(RequestId, nil);
    FClient.Send(Query.JSON, False);
  finally
    Query.Free;
  end;
end;

procedure TDLibMethods.GetMe(Callback: TProc<TtgUser>);
begin
  Execute<TtgUser>(TGetMe.Create, '', Callback);
end;

function TDLibMethods.NewRequestId: TRequestId;
begin
  Inc(FRequestQueue);
  Result := FRequestQueue;
end;

function TDLibMethods.Proc(const RequestId: TRequestId; JSON: TJSONObject): Boolean;
var
  Callback: TProc<TJSONObject>;
  Dict: TSafeDictionary<TRequestId, TProc<TJSONObject>>;
begin
  Dict := FPoll.Lock;
  try
    if Dict.TryGetValue(RequestId, Callback) then
    begin
      Dict.Remove(RequestId);
      if Assigned(Callback) then
        Callback(JSON);
      Exit(True);
    end;
  finally
    FPoll.Unlock;
  end;
  Result := False;
end;

procedure TDLibMethods.SendMessage(Params: TBuildSendMessage; Callback: TProc<TtgMessage>);
begin
  Execute<TtgMessage>(Params, '', Callback);
end;

procedure TDLibMethods.SetSync(const Value: Boolean);
begin
  FSync := Value;
end;

procedure TDLibMethods.Sync(Proc: TProc);
begin
  TThread.Queue(nil,
    procedure
    begin
      Proc;
    end);
end;

end.

