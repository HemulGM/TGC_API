unit TGC.Entity.User;

interface

uses
  Rest.Json.Types, Rest.JsonReflect, Rest.Json.Interceptors;

type
  TtgUserType = class
  private
    [JSONNameAttribute('@type')]
    FType: string;
  public
    property &Type: string read FType write FType;
  end;

  TtgUserStatus = class
  private
    [JSONNameAttribute('@type')]
    Ftype: string;
    [JsonReflect(ctString, rtString, TUnixDateTimeInterceptor)]
    FWas_online: TDateTime;
  public
    property &Type: string read Ftype write Ftype;
    property WasOnline: TDateTime read FWas_online write FWas_online;
  end;

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
  public
    property AddedToAttachmentMenu: Boolean read FAdded_to_attachment_menu write FAdded_to_attachment_menu;
    property FirstName: string read FFirst_name write FFirst_name;
    property HaveAccess: Boolean read FHave_access write FHave_access;
    property Id: Int64 read FId write FId;
    property IsContact: Boolean read FIs_contact write FIs_contact;
    property IsFake: Boolean read FIs_fake write FIs_fake;
    property IsMutualContact: Boolean read FIs_mutual_contact write FIs_mutual_contact;
    property IsPremium: Boolean read FIs_premium write FIs_premium;
    property IsScam: Boolean read FIs_scam write FIs_scam;
    property IsSupport: Boolean read FIs_support write FIs_support;
    property IsVerified: Boolean read FIs_verified write FIs_verified;
    property LanguageCode: string read FLanguage_code write FLanguage_code;
    property LastName: string read FLast_name write FLast_name;
    property PhoneNumber: string read FPhone_number write FPhone_number;
    property RestrictionReason: string read FRestriction_reason write FRestriction_reason;
    property Status: TtgUserStatus read FStatus write FStatus;
    property UserType: TtgUserType read FType write FType;
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
  inherited;
end;

end.

