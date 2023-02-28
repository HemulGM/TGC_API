unit TGC.Wrapper;

interface

type
  TGChar = PAnsiChar;

  TVoid = IntPtr;

const
  {$IFDEF MSWINDOWS}
  TDLibDLLName: string = 'tdjson.dll';
  {$ELSE}
  TDLibDLLName: string = 'libtdjson.so';
  {$ENDIF}

type
  /// <summary>
  /// A type of callback function that will be called when a message is added to the internal TDLib log.
  /// </summary>
  TLogMessageCallback = procedure(ErrorMessage: TGChar);

  /// <summary>
  /// Creates a new instance of TDLib.
  /// </summary>
  /// <returns>Pointer to the created instance of TDLib.</returns>
  TJsonCreateClient = function(): TVoid; cdecl;

  /// <summary>
  /// Sends request to the TDLib client. May be called from any thread.
  /// </summary>
  /// <param name="ClientId: TVoid">The client.</param>
  /// <param name="Request: TGChar">JSON-serialized null-terminated request to TDLib.</param>
  TJsonClientSend = procedure(Client: TVoid; Request: TGChar); cdecl;

  /// <summary>
  /// Receives incoming updates and request responses from the TDLib client. May be called from any thread, but must not be called simultaneously from two different threads. Returned pointer will be deallocated by TDLib during next call to td_json_client_receive or td_json_client_execute in the same thread, so it can't be used after that.
  /// </summary>
  /// <param name="Client: TVoid">The client.</param>
  /// <param name="Timeout: Double">The maximum number of seconds allowed for this function to wait for new data.</param>
  /// <returns>JSON-serialized null-terminated incoming update or request response. May be NULL if the timeout expires.</returns>
  TJsonClientReceive = function(Client: TVoid; Timeout: Double): TGChar; cdecl;

  /// <summary>
  /// Synchronously executes TDLib request. May be called from any thread. Only a few requests can be executed synchronously. Returned pointer will be deallocated by TDLib during next call to td_json_client_receive or td_json_client_execute in the same thread, so it can't be used after that.
  /// </summary>
  /// <param name="Client: TVoid">The client. Currently ignored for all requests, so NULL can be passed.</param>
  /// <param name="Request: TGChar">JSON-serialized null-terminated request to TDLib.</param>
  /// <returns>JSON-serialized null-terminated request response.</returns>
  TJsonClientExecute = function(Client: TVoid; Request: TGChar): TGChar; cdecl;

  /// <summary>
  /// Destroys the TDLib client instance. After this is called the client instance must not be used anymore.
  /// </summary>
  /// <param name="Client: TVoid">The client.</param>
  TJsonClientDestroy = procedure(Client: TVoid); cdecl;

  /// <summary>
  /// Sets the callback that will be called when a message is added to the internal TDLib log. None of the TDLib methods can be called from the callback. By default the callback is not set.
  /// </summary>
  /// <param name="MaxVerbosityLevel: Integer">The maximum verbosity level of messages for which the callback will be called.</param>
  /// <param name="Callback: TLogMessageCallback">Callback that will be called when a message is added to the internal TDLib log. Pass nullptr to remove the callback.</param>
  TSetLogMessageCallback = procedure(MaxVerbosityLevel: Integer; Callback: TLogMessageCallback); cdecl;

var
  /// <summary>
  /// Creates a new instance of TDLib.
  /// </summary>
  /// <returns>Pointer to the created instance of TDLib.</returns>
  JsonCreateClient: TJsonCreateClient;
  /// <summary>
  /// Destroys the TDLib client instance. After this is called the client instance must not be used anymore.
  /// </summary>
  /// <param name="Client: TVoid">The client.</param>
  JsonClientDestroy: TJsonClientDestroy;
  /// <summary>
  /// Sends request to the TDLib client. May be called from any thread.
  /// </summary>
  /// <param name="ClientId: TVoid">The client.</param>
  /// <param name="Request: TGChar">JSON-serialized null-terminated request to TDLib.</param>
  JsonClientSend: TJsonClientSend;
  /// <summary>
  /// Receives incoming updates and request responses from the TDLib client. May be called from any thread, but must not be called simultaneously from two different threads. Returned pointer will be deallocated by TDLib during next call to td_json_client_receive or td_json_client_execute in the same thread, so it can't be used after that.
  /// </summary>
  /// <param name="Client: TVoid">The client.</param>
  /// <param name="Timeout: Double">The maximum number of seconds allowed for this function to wait for new data.</param>
  /// <returns>JSON-serialized null-terminated incoming update or request response. May be NULL if the timeout expires.</returns>
  JsonClientReceive: TJsonClientReceive;
  /// <summary>
  /// Synchronously executes TDLib request. May be called from any thread. Only a few requests can be executed synchronously. Returned pointer will be deallocated by TDLib during next call to td_json_client_receive or td_json_client_execute in the same thread, so it can't be used after that.
  /// </summary>
  /// <param name="Client: TVoid">The client. Currently ignored for all requests, so NULL can be passed.</param>
  /// <param name="Request: TGChar">JSON-serialized null-terminated request to TDLib.</param>
  /// <returns>JSON-serialized null-terminated request response.</returns>
  JsonClientExecute: TJsonClientExecute;
  /// <summary>
  /// Sets the callback that will be called when a message is added to the internal TDLib log. None of the TDLib methods can be called from the callback. By default the callback is not set.
  /// </summary>
  /// <param name="MaxVerbosityLevel: Integer">The maximum verbosity level of messages for which the callback will be called.</param>
  /// <param name="Callback: TLogMessageCallback">Callback that will be called when a message is added to the internal TDLib log. Pass nullptr to remove the callback.</param>
  SetLogMessageCallback: TSetLogMessageCallback;

var
  TDLib: NativeInt = 0;

function TDLibInitialize: Boolean;

procedure TDLibFinalize;

function StringToTgChar(const Value: string): TGChar;

function TgCharToString(const Value: TGChar): string;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils;

var
  TDLibRefCount: Integer = 0;

function StringToTgChar(const Value: string): TGChar;
begin
  Result := TGChar(UTF8Encode(Value));
end;

function TgCharToString(const Value: TGChar): string;
begin
  Result := UTF8ToString(Value);
end;

function TDLibInitialize: Boolean;
var
  FilePath: string;
begin
  FilePath := TDLibDLLName;
  if TDLib = 0 then
    TDLib := SafeLoadLibrary(FilePath);
  if TDLib <> 0 then
  begin
    @JsonCreateClient := GetProcAddress(TDLib, 'td_json_client_create');
    @JsonClientDestroy := GetProcAddress(TDLib, 'td_json_client_destroy');
    @JsonClientSend := GetProcAddress(TDLib, 'td_json_client_send');
    @JsonClientReceive := GetProcAddress(TDLib, 'td_json_client_receive');
    @JsonClientExecute := GetProcAddress(TDLib, 'td_json_client_execute');
    @SetLogMessageCallback := GetProcAddress(TDLib, 'td_set_log_fatal_error_callback');
  end;
  Result := TDLib <> 0;
  if Result then
    Inc(TDLibRefCount);
end;

procedure TDLibFinalize;
begin
  Dec(TDLibRefCount);
  if TDLibRefCount <= 0 then
    if TDLib <> 0 then
    begin
      FreeLibrary(TDLib);
      TDLib := 0;
    end;
end;

end.

