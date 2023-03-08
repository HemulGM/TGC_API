unit TGC.Builder.GetUser;

interface

uses
  TGC.Classes;

type
  /// <summary>
  /// Returns information about a user by their identifier. This is an offline request if the current user is not a bot.
  /// </summary>
  TGetUser = class(TParam)
    constructor Create; reintroduce;
    /// <summary>
    /// User identifier.
    /// </summary>
    function UserId(const Value: Int64): TGetUser;
  end;

implementation

{ TGetUser }

constructor TGetUser.Create;
begin
  inherited Create('getUser');
end;

function TGetUser.UserId(const Value: Int64): TGetUser;
begin
  Result := TGetUser(Add('user_id', Value));
end;

end.

