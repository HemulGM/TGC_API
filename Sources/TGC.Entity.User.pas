unit TGC.Entity.User;

interface

uses
  Rest.Json.Types, Rest.JsonReflect, Rest.Json.Interceptors, TGC.Entity.Files, TGC.Entity.ProfilePhoto;

type
  /// <summary>
  /// Represents the type of a user. The following types are possible: regular users, deleted users and bots.
  /// </summary>
  TtgUserType = class
  private
    [JSONNameAttribute('@type')]
    FType: string;
  public
    /// <summary>
    /// userTypeBot, userTypeDeleted, userTypeRegular, and userTypeUnknown
    /// </summary>
    property AType: string read FType write FType;
  end;

  /// <summary>
  /// Describes the last time the user was online.
  /// </summary>
  TtgUserStatus = class
  private
    [JSONNameAttribute('@type')]
    Ftype: string;
    [JsonReflect(ctString, rtString, TUnixDateTimeInterceptor)]
    FWas_online: TDateTime;
  public
    /// <summary>
    /// userStatusEmpty, userStatusLastMonth, userStatusLastWeek, userStatusOffline, userStatusOnline, and userStatusRecently
    /// </summary>
    property AType: string read Ftype write Ftype;
    property WasOnline: TDateTime read FWas_online write FWas_online;
  end;

  /// <summary>
  /// User names
  /// </summary>
  TtgUserNames = class
  private
    FActive_usernames: TArray<string>;
    FDisabled_usernames: TArray<string>;
    FEditable_username: string;
  public
    property ActiveUsernames: TArray<string> read FActive_usernames write FActive_usernames;
    property DisabledUsernames: TArray<string> read FDisabled_usernames write FDisabled_usernames;
    property EditableUsername: string read FEditable_username write FEditable_username;
  end;

  /// <summary>
  /// Represents a user.
  /// </summary>
  TtgUser = class
  private
    FAdded_to_attachment_menu: Boolean;
    FFirst_name: string;
    FHave_access: Boolean;
    FId: Int64;
    FIs_contact: Boolean;
    FIs_fake: Boolean;
    FIs_mutual_contact: Boolean;
    FIs_premium: Boolean;
    FIs_scam: Boolean;
    FIs_support: Boolean;
    FIs_verified: Boolean;
    FLanguage_code: string;
    FLast_name: string;
    FPhone_number: string;
    FRestriction_reason: string;
    FStatus: TtgUserStatus;
    FType: TtgUserType;
    FProfile_photo: TTgProfilePhoto;
    FUsernames: TtgUserNames;
    function GetType: string;
  public
    property AddedToAttachmentMenu: Boolean read FAdded_to_attachment_menu write FAdded_to_attachment_menu;
    /// <summary>
    /// First name of the user.
    /// </summary>
    property FirstName: string read FFirst_name write FFirst_name;
    /// <summary>
    /// If false, the user is inaccessible, and the only information known about the user is inside this class. It can't be passed to any method except GetUser.
    /// </summary>
    property HaveAccess: Boolean read FHave_access write FHave_access;
    /// <summary>
    /// User identifier.
    /// </summary>
    property Id: Int64 read FId write FId;
    /// <summary>
    /// The user is a contact of the current user.
    /// </summary>
    property IsContact: Boolean read FIs_contact write FIs_contact;
    /// <summary>
    /// True, if many users reported this user as a fake account.
    /// </summary>
    property IsFake: Boolean read FIs_fake write FIs_fake;
    /// <summary>
    /// The user is a contact of the current user and the current user is a contact of the user.
    /// </summary>
    property IsMutualContact: Boolean read FIs_mutual_contact write FIs_mutual_contact;
    /// <summary>
    /// /// The user have Premium
    /// </summary>
    property IsPremium: Boolean read FIs_premium write FIs_premium;
    /// <summary>
    /// True, if many users reported this user as a scam.
    /// </summary>
    property IsScam: Boolean read FIs_scam write FIs_scam;
    /// <summary>
    /// True, if the user is Telegram support account.
    /// </summary>
    property IsSupport: Boolean read FIs_support write FIs_support;
    /// <summary>
    /// True, if the user is verified.
    /// </summary>
    property IsVerified: Boolean read FIs_verified write FIs_verified;
    /// <summary>
    /// IETF language tag of the user's language; only available to bots.
    /// </summary>
    property LanguageCode: string read FLanguage_code write FLanguage_code;
    /// <summary>
    /// Last name of the user.
    /// </summary>
    property LastName: string read FLast_name write FLast_name;
    /// <summary>
    /// Phone number of the user.
    /// </summary>
    property PhoneNumber: string read FPhone_number write FPhone_number;
    /// <summary>
    /// Profile photo of the user; may be null.
    /// </summary>
    property ProfilePhoto: TTgProfilePhoto read FProfile_photo write FProfile_photo;
    /// <summary>
    /// If non-empty, it contains a human-readable description of the reason why access to this user must be restricted.
    /// </summary>
    property RestrictionReason: string read FRestriction_reason write FRestriction_reason;
    /// <summary>
    /// Current online status of the user.
    /// </summary>
    property Status: TtgUserStatus read FStatus write FStatus;
    /// <summary>
    /// Type of the user. userTypeBot, userTypeDeleted, userTypeRegular, and userTypeUnknown
    /// </summary>
    property UserType: string read GetType;
    /// <summary>
    /// Username of the user.
    /// </summary>
    property UserNames: TtgUserNames read FUsernames write FUsernames;
    destructor Destroy; override;
  end;

implementation

{ TtgUser }

destructor TtgUser.Destroy;
begin
  if Assigned(FStatus) then
    FStatus.Free;
  if Assigned(FType) then
    FType.Free;
  if Assigned(FUsernames) then
    FUsernames.Free;
  if Assigned(FProfile_photo) then
    FProfile_photo.Free;
  inherited;
end;

function TtgUser.GetType: string;
begin
  if Assigned(FType) then
    Result := FType.FType;
end;

end.

