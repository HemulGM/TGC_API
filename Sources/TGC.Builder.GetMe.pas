unit TGC.Builder.GetMe;

interface

uses
  TGC.Classes;

type
  TBuildGetMe = class(TParam)
    constructor Create; reintroduce;
  end;

implementation

{ TGetMe }

constructor TBuildGetMe.Create;
begin
  inherited Create('getMe');
end;

end.

