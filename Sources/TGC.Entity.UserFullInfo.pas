unit TGC.Entity.UserFullInfo;

interface

uses
  Rest.Json.Types, Rest.JsonReflect, Rest.Json.Interceptors, TGC.Entity.AObject,
  TGC.Entity.ChatPhoto, TGC.Entity.BotCommand;

type
  /// <summary>
  /// Contains full information about a user.
  /// </summary>
  TtgUserFullInfo = class(TtgObject)
  private
    FPhoto: TtgChatPhoto;
    FIs_blocked: Boolean;
    FCan_be_called: Boolean;
    FSupports_video_calls: Boolean;
    FHas_private_calls: Boolean;
    FHas_private_forwards: Boolean;
    FNeed_phone_number_privacy_exception: Boolean;
    FBio: string;
    FShare_text: string;
    FDescription: string;
    FGroup_in_common_count: Int32;
    FCommands: TArray<TtgBotCommand>;
  public
    /// <summary>
    /// User profile photo; may be null.
    /// </summary>
    property Photo: TtgChatPhoto read FPhoto write FPhoto;
    /// <summary>
    /// True, if the user is blocked by the current user.
    /// </summary>
    property IsBlocked: Boolean read FIs_blocked write FIs_blocked;
    /// <summary>
    /// True, if the user can be called.
    /// </summary>
    property CanBeCalled: Boolean read FCan_be_called write FCan_be_called;
    /// <summary>
    /// True, if a video call can be created with the user.
    /// </summary>
    property SupportsVideoCalls: Boolean read FSupports_video_calls write FSupports_video_calls;
    /// <summary>
    /// True, if the user can't be called due to their privacy settings.
    /// </summary>
    property HasPrivateCalls: Boolean read FHas_private_calls write FHas_private_calls;
    /// <summary>
    /// True, if the user can't be linked in forwarded messages due to their privacy settings.
    /// </summary>
    property HasPrivateForwards: Boolean read FHas_private_forwards write FHas_private_forwards;
    /// <summary>
    /// True, if the current user needs to explicitly allow to share their phone number with the user when the method addContact is used.
    /// </summary>
    property NeedPhoneNumberPrivacyException: Boolean read FNeed_phone_number_privacy_exception write FNeed_phone_number_privacy_exception;
    /// <summary>
    /// A short user bio.
    /// </summary>
    property Bio: string read FBio write FBio;
    /// <summary>
    /// For bots, the text that is shown on the bot's profile page and is sent together with the link when users share the bot.
    /// </summary>
    property ShareText: string read FShare_text write FShare_text;
    /// <summary>
    /// For bots, the text shown in the chat with the bot if the chat is empty.
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Number of group chats where both the other user and the current user are a member; 0 for the current user.
    /// </summary>
    property GroupInCommonCount: Int32 read FGroup_in_common_count write FGroup_in_common_count;
    /// <summary>
    /// For bots, list of the bot commands.
    /// </summary>
    property Commands: TArray<TtgBotCommand> read FCommands write FCommands;
    destructor Destroy; override;
  end;

implementation

{ TtgUserFullInfo }

destructor TtgUserFullInfo.Destroy;
begin
  FPhoto.Free;
  for var Item in FCommands do
    Item.Free;
  inherited;
end;

end.

