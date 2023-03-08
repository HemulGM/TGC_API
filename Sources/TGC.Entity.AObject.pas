unit TGC.Entity.AObject;

interface
  //'{"@type":"error","code":400,"message":"There are no messages to send","@extra":1}'

uses
  REST.Json.Types;

type
  TtgObject = class
  private
    [JSONName('@type')]
    FAType: string;
    FCode: Integer;
    FMessage: string;
    function GetIsError: Boolean;
  public
    property AType: string read FAType write FAType;
    property Code: Integer read FCode write FCode;
    property Message: string read FMessage write FMessage;
    property IsError: Boolean read GetIsError;
  end;

implementation

{ TtgObject }

function TtgObject.GetIsError: Boolean;
begin
  Result := AType = 'error';
end;

end.

