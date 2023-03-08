unit TGC.Builder.GetUserFullInfo;

interface

uses
  TGC.Classes;

type
  /// <summary>
  /// Returns full information about a user by their identifier.
  /// </summary>
  TGetUserFullInfo = class(TParam)
    constructor Create; reintroduce;
    /// <summary>
    /// User identifier.
    /// </summary>
    function UserId(const Value: Int64): TGetUserFullInfo;
  end;

implementation

{ TGetUserFullInfo }

constructor TGetUserFullInfo.Create;
begin
  inherited Create('getUserFullInfo');
end;

function TGetUserFullInfo.UserId(const Value: Int64): TGetUserFullInfo;
begin
  Result := TGetUserFullInfo(Add('user_id', Value));
end;

end.

