unit TGC.Handler;

interface

uses
  System.Classes, System.JSON;
//{"@type":"updateOption","name":"version","value":{"@type":"optionValueString","value":"1.7.0"}}

type
  THandler = class
  private
    FClient: TObject;
  public
    procedure Execute(JSON: TJSONObject); virtual; abstract;
    constructor Create(AClient: TObject); virtual;
    property Client: TObject read FClient;
  end;

implementation

{ THandler }

constructor THandler.Create(AClient: TObject);
begin
  inherited Create;
  FClient := AClient;
end;

end.

