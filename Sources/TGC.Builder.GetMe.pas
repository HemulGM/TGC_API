unit TGC.Builder.GetMe;

interface

uses
  TGC.Classes;

type
  /// <summary>
  /// Returns the current user.
  /// </summary>
  TGetMe = class(TParam)
    constructor Create; reintroduce;
  end;

implementation

{ TGetMe }

constructor TGetMe.Create;
begin
  inherited Create('getMe');
end;

end.

