unit TGC.Classes;

interface

uses
  System.Classes, HGM.JSONParams, System.Generics.Collections, System.SysUtils;

type
  TThreadObject<T: class, constructor> = class
  private
    FLock: TObject;
  protected
    FObject: T;
  public
    constructor Create;
    destructor Destroy; override;
    function Lock: T;
    procedure Locked(Proc: TProc<T>);
    procedure Unlock; inline;
  end;

  TSafeDictionary<KeyType, ValueType> = class(TDictionary<KeyType, ValueType>)
    constructor Create;
  end;

  TThreadDict<KeyType, ValueType> = class(TThreadObject<TSafeDictionary<KeyType, ValueType>>)
  public
    procedure Add(const Key: KeyType; const Item: ValueType);
    procedure Clear;
    procedure Remove(const Key: KeyType); inline;
  end;

  TParam = class(TJSONParam)
    function Extra(const Value: Int64): TParam;
    constructor Create(AType: string); reintroduce;
  end;

  TGetMe = class(TParam)
    constructor Create; reintroduce;
  end;

implementation

{ TParam }

constructor TParam.Create(AType: string);
begin
  inherited Create;
  Add('@type', AType);
end;

function TParam.Extra(const Value: Int64): TParam;
begin
  Result := TParam(Add('@extra', Value));
end;

{ TGetMe }

constructor TGetMe.Create;
begin
  inherited Create('getMe');
end;

{ TThreadObject<T> }

constructor TThreadObject<T>.Create;
begin
  if T.ClassName.StartsWith('TDictionary<') then
    raise Exception.Create('Not allow default TDictionary');
  inherited Create;
  FLock := TObject.Create;
  FObject := T.Create;
end;

destructor TThreadObject<T>.Destroy;
begin
  Lock;
  try
    FObject.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
end;

function TThreadObject<T>.Lock: T;
begin
  TMonitor.Enter(FLock);
  Result := FObject;
end;

procedure TThreadObject<T>.Locked(Proc: TProc<T>);
begin
  Lock;
  try
    Proc(FObject);
  finally
    Unlock;
  end;
end;

procedure TThreadObject<T>.Unlock;
begin
  TMonitor.Exit(FLock);
end;
{ TThreadDict<TKey, ValueType> }

procedure TThreadDict<KeyType, ValueType>.Add(const Key: KeyType; const Item: ValueType);
begin
  Lock;
  try
    FObject.AddOrSetValue(Key, Item);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<KeyType, ValueType>.Clear;
begin
  Lock;
  try
    FObject.Clear;
  finally
    Unlock;
  end;
end;

procedure TThreadDict<KeyType, ValueType>.Remove(const Key: KeyType);
begin
  Lock;
  try
    Remove(Key);
  finally
    Unlock;
  end;
end;
{ TSafeDictionary<KeyType, ValueType> }

constructor TSafeDictionary<KeyType, ValueType>.Create;
begin
  inherited Create(0);
end;

end.

