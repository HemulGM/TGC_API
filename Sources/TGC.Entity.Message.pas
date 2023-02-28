unit TGC.Entity.Message;

interface

uses
  TGC.Entity.Files, REST.JsonReflect, REST.Json.Interceptors, TGC.Entity.Sticker,
  REST.Json.Types, TGC.Entity.FormatedText;

type
  /// <summary>
  /// Contains the content of a message
  /// </summary>
  TtgMessageContent = class
  private
    [JSONName('@type')]
    Ftype: string;
    FAnimated_emoji: TtgAnimatedEmoji;
    FEmoji: string;
    FText: TtgFormatedText;
  public
    property AType: string read Ftype write Ftype;
    /// <summary>
    /// The animated emoji.
    /// </summary>
    property AnimatedEmoji: TtgAnimatedEmoji read FAnimated_emoji write FAnimated_emoji;
    /// <summary>
    /// The corresponding emoji.
    /// </summary>
    property Emoji: string read FEmoji write FEmoji;
    /// <summary>
    ///  A text message.
    /// </summary>
    property Text: TtgFormatedText read FText write FText;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Contains information about the sender of a message
  /// </summary>
  TtgMessageSender = class
  private
    [JSONName('@type')]
    Ftype: string;
    FUser_id: Int64;
    FChat_id: Int64;
  public
    /// <summary>
    /// messageSenderChat, and messageSenderUser
    /// </summary>
    property AType: string read Ftype write Ftype;
    /// <summary>
    /// Identifier of the user that sent the message.
    /// </summary>
    property UserId: Int64 read FUser_id write FUser_id;
    /// <summary>
    /// Identifier of the chat that sent the message.
    /// </summary>
    property ChatId: Int64 read FChat_id write FChat_id;
  end;

  /// <summary>
  /// Describes a message.
  /// </summary>
  TtgMessage = class
  private
    FAuthor_signature: string;
    FAuto_delete_in: Extended;
    FCan_be_deleted_for_all_users: Boolean;
    FCan_be_deleted_only_for_self: Boolean;
    FCan_be_edited: Boolean;
    FCan_be_forwarded: Boolean;
    FCan_be_saved: Boolean;
    FCan_get_added_reactions: Boolean;
    FCan_get_media_timestamp_links: Boolean;
    FCan_get_message_thread: Boolean;
    FCan_get_statistics: Boolean;
    FCan_get_viewers: Boolean;
    FCan_report_reactions: Boolean;
    FChat_id: Int64;
    FContains_unread_mention: Boolean;
    FContent: TtgMessageContent;
    [JsonReflect(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    [JsonReflect(ctString, rtString, TUnixDateTimeInterceptor)]
    FEdit_date: TDateTime;
    FHas_timestamped_media: Boolean;
    FId: Int64;
    FIs_channel_post: Boolean;
    FIs_outgoing: Boolean;
    FIs_pinned: Boolean;
    FIs_topic_message: Boolean;
    FMedia_album_id: string;
    FMessage_thread_id: Int64;
    FReply_in_chat_id: Int64;
    FReply_to_message_id: Int64;
    FRestriction_reason: string;
    FSelf_destruct_in: Extended;
    FSelf_destruct_time: Extended;
    FSender_id: TtgMessageSender;
    FUnread_reactions: TArray<string>;
    FVia_bot_user_id: Int64;
    FTtl: Int64;
    FTtl_expires_in: Extended;
  public
    /// <summary>
    /// For channel posts and anonymous group messages, optional author signature.
    /// </summary>
    property AuthorSignature: string read FAuthor_signature write FAuthor_signature;
    property AutoDeleteIn: Extended read FAuto_delete_in write FAuto_delete_in;
    /// <summary>
    /// True, if the message can be deleted for all users.
    /// </summary>
    property CanBeDeletedForAllUsers: Boolean read FCan_be_deleted_for_all_users write FCan_be_deleted_for_all_users;
    /// <summary>
    /// True, if the message can be deleted only for the current user while other users will continue to see it.
    /// </summary>
    property CanBeDeletedOnlyForSelf: Boolean read FCan_be_deleted_only_for_self write FCan_be_deleted_only_for_self;
    /// <summary>
    /// True, if the message can be edited. For live location and poll messages this fields shows whether editMessageLiveLocation or stopPoll can be used with this message by the application.
    /// </summary>
    property CanBeEdited: Boolean read FCan_be_edited write FCan_be_edited;
    /// <summary>
    /// True, if the message can be forwarded.
    /// </summary>
    property CanBeForwarded: Boolean read FCan_be_forwarded write FCan_be_forwarded;
    /// <summary>
    /// True, if content of the message can be saved locally or copied.
    /// </summary>
    property CanBeSaved: Boolean read FCan_be_saved write FCan_be_saved;
    property CanFetAddedReactions: Boolean read FCan_get_added_reactions write FCan_get_added_reactions;
    /// <summary>
    /// True, if media timestamp links can be generated for media timestamp entities in the message text, caption or web page description.
    /// </summary>
    property CanGetMediaTimestampLinks: Boolean read FCan_get_media_timestamp_links write FCan_get_media_timestamp_links;
    /// <summary>
    /// True, if the message thread info is available.
    /// </summary>
    property CanGetMessageThread: Boolean read FCan_get_message_thread write FCan_get_message_thread;
    /// <summary>
    /// True, if the message statistics are available.
    /// </summary>
    property CanGetStatistics: Boolean read FCan_get_statistics write FCan_get_statistics;
    /// <summary>
    /// True, if chat members already viewed the message can be received through getMessageViewers.
    /// </summary>
    property CanGetViewers: Boolean read FCan_get_viewers write FCan_get_viewers;
    property CanReportReactions: Boolean read FCan_report_reactions write FCan_report_reactions;
    /// <summary>
    /// Chat identifier.
    /// </summary>
    property ChatId: Int64 read FChat_id write FChat_id;
    /// <summary>
    /// True, if the message contains an unread mention for the current user.
    /// </summary>
    property ContainsUnreadMention: Boolean read FContains_unread_mention write FContains_unread_mention;
    /// <summary>
    /// Content of the message.
    /// </summary>
    property Content: TtgMessageContent read FContent write FContent;
    /// <summary>
    /// Point in time (Unix timestamp) when the message was sent.
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Point in time (Unix timestamp) when the message was last edited.
    /// </summary>
    property EditDate: TDateTime read FEdit_date write FEdit_date;
    /// <summary>
    /// True, if media timestamp entities refers to a media in this message as opposed to a media in the replied message.
    /// </summary>
    property HasTimestampedMedia: Boolean read FHas_timestamped_media write FHas_timestamped_media;
    /// <summary>
    /// Message identifier; unique for the chat to which the message belongs.
    /// </summary>
    property Id: Int64 read FId write FId;
    /// <summary>
    /// True, if the message is a channel post. All messages to channels are channel posts, all other messages are not channel posts.
    /// </summary>
    property IsChannelPost: Boolean read FIs_channel_post write FIs_channel_post;
    /// <summary>
    /// True, if the message is outgoing.
    /// </summary>
    property IsOutgoing: Boolean read FIs_outgoing write FIs_outgoing;
    /// <summary>
    /// True, if the message is pinned.
    /// </summary>
    property IsPinned: Boolean read FIs_pinned write FIs_pinned;
    property IsTopicMessage: Boolean read FIs_topic_message write FIs_topic_message;
    /// <summary>
    /// Unique identifier of an album this message belongs to. Only audios, documents, photos and videos can be grouped together in albums.
    /// </summary>
    property MediaAlbumId: string read FMedia_album_id write FMedia_album_id;
    /// <summary>
    /// If non-zero, the identifier of the message thread the message belongs to; unique within the chat to which the message belongs.
    /// </summary>
    property MessageThreadId: Int64 read FMessage_thread_id write FMessage_thread_id;
    /// <summary>
    /// If non-zero, the identifier of the chat to which the replied message belongs; Currently, only messages in the Replies chat can have different reply_in_chat_id and chat_id.
    /// </summary>
    property ReplyInChatId: Int64 read FReply_in_chat_id write FReply_in_chat_id;
    /// <summary>
    /// If non-zero, the identifier of the message this message is replying to; can be the identifier of a deleted message.
    /// </summary>
    property ReplyToMessageId: Int64 read FReply_to_message_id write FReply_to_message_id;
    /// <summary>
    /// If non-empty, contains a human-readable description of the reason why access to this message must be restricted.
    /// </summary>
    property RestrictionReason: string read FRestriction_reason write FRestriction_reason;
    property SelfDestructIn: Extended read FSelf_destruct_in write FSelf_destruct_in;
    property SelfDestructRime: Extended read FSelf_destruct_time write FSelf_destruct_time;
    /// <summary>
    /// Identifier of the sender of the message.
    /// </summary>
    property SenderId: TtgMessageSender read FSender_id write FSender_id;
    property UnreadReactions: TArray<string> read FUnread_reactions write FUnread_reactions;
    /// <summary>
    /// If non-zero, the user identifier of the bot through which this message was sent.
    /// </summary>
    property ViaBotUserId: Int64 read FVia_bot_user_id write FVia_bot_user_id;
    /// <summary>
    /// For self-destructing messages, the message's TTL (Time To Live), in seconds; 0 if none. TDLib will send updateDeleteMessages or updateMessageContent once the TTL expires.
    /// </summary>
    property TTL: Int64 read FTtl write FTtl;
    /// <summary>
    /// Time left before the message expires, in seconds. If the TTL timer isn't started yet, equals to the value of the ttl field.
    /// </summary>
    property TTLExpiresIn: Extended read FTtl_expires_in write FTtl_expires_in;
    destructor Destroy; override;
  end;

implementation

{ TtgMessageContent }

destructor TtgMessageContent.Destroy;
begin
  FAnimated_emoji.Free;
  FText.Free;
  inherited;
end;

{ TtgMessage }

destructor TtgMessage.Destroy;
begin
  FSender_id.free;
  FContent.free;
  inherited;
end;

end.

