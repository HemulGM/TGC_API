unit TGC.Classes;

interface

uses
  HGM.JSONParams;

type
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

end.

