unit TGC.Options;

interface

type
  TtgOptions = class
  private
    FVersion: string;
    FCommit_hash: string;
    FUnix_time: Int64;
    FUtc_time_offset: Int64;
    FMessage_text_length_max: Int64;
    FMessage_caption_length_max: Int64;
    FBio_length_max: Int64;
    FSuggested_video_note_length: Int64;
    FSuggested_video_note_video_bitrate: Int64;
    FSuggested_video_note_audio_bitrate: Int64;
    FNotification_sound_duration_max: Int64;
    FNotification_sound_size_max: Int64;
    FNotification_sound_count_max: Int64;
    FChat_filter_count_max: Int64;
    FChat_filter_chosen_chat_count_max: Int64;
    FPinned_forum_topic_count_max: Int64;
    FTelegram_service_notifications_chat_id: Int64;
    FReplies_bot_chat_id: Int64;
    FGroup_anonymous_bot_user_id: Int64;
    FChannel_bot_user_id: Int64;
    FAnti_spam_bot_user_id: Int64;
    FIs_location_visible: Boolean;
    FFavorite_stickers_limit: Int64;
    FTest_mode: Boolean;
    FForwarded_message_count_max: Int64;
    FBasic_group_size_max: Int64;
    FSupergroup_size_max: Int64;
    FPinned_chat_count_max: Int64;
    FPinned_archived_chat_count_max: Int64;
    FExpect_blocking: Boolean;
    FT_me_url: string;
    FCalls_enabled: Boolean;
    FCall_connect_timeout_ms: Int64;
    FCall_packet_timeout_ms: Int64;
    FAnimation_search_bot_username: string;
    FVenue_search_bot_username: string;
    FPhoto_search_bot_username: string;
    FAuthorization_date: Int64;
    FMy_id: Int64;
    FArchive_and_mute_new_chats_from_unknown_users: Boolean;
    FIgnore_sensitive_content_restrictions: Boolean;
    FCan_ignore_sensitive_content_restrictions: Boolean;
    FIs_premium_available: Boolean;
    FAuthentication_token: string;
  public
    property Version: string read FVersion;
    property CommitHash: string read FCommit_hash;
    property UnixTime: Int64 read FUnix_time;
    property UtcTimeOffset: Int64 read FUtc_time_offset;
    property MessageTextLengthMax: Int64 read FMessage_text_length_max;
    property MessageCaptionLengthMax: Int64 read FMessage_caption_length_max;
    property BioLengthMax: Int64 read FBio_length_max;
    property SuggestedVideoNoteLength: Int64 read FSuggested_video_note_length;
    property SuggestedVideoNoteVideoBitrate: Int64 read FSuggested_video_note_video_bitrate;
    property SuggestedVideoNoteAudioBitrate: Int64 read FSuggested_video_note_audio_bitrate;
    property NotificationSoundDurationMax: Int64 read FNotification_sound_duration_max;
    property NotificationSoundSizeMax: Int64 read FNotification_sound_size_max;
    property NotificationSoundCountMax: Int64 read FNotification_sound_count_max;
    property ChatFilterCountMax: Int64 read FChat_filter_count_max;
    property ChatFilterChosenChatCountMax: Int64 read FChat_filter_chosen_chat_count_max;
    property PinnedForumTopicCountMax: Int64 read FPinned_forum_topic_count_max;
    property TelegramServiceNotificationsChatId: Int64 read FTelegram_service_notifications_chat_id;
    property RepliesBotChatId: Int64 read FReplies_bot_chat_id;
    property GroupAnonymousBotUserId: Int64 read FGroup_anonymous_bot_user_id;
    property ChannelBotUserId: Int64 read FChannel_bot_user_id;
    property AntiSpamBotUserId: Int64 read FAnti_spam_bot_user_id;
    property IsLocationVisible: Boolean read FIs_location_visible;
    property FavoriteStickersLimit: Int64 read FFavorite_stickers_limit;
    property TestMode: Boolean read FTest_mode;
    property ForwardedMessageCountMax: Int64 read FForwarded_message_count_max;
    property BasicGroupSizeMax: Int64 read FBasic_group_size_max;
    property SupergroupSizeMax: Int64 read FSupergroup_size_max;
    property PinnedChatCountMax: Int64 read FPinned_chat_count_max;
    property PinnedArchivedChatCountMax: Int64 read FPinned_archived_chat_count_max;
    property ExpectBlocking: Boolean read FExpect_blocking;
    property TMeUrl: string read FT_me_url;
    property CallsEnabled: Boolean read FCalls_enabled;
    property CallConnectTimeoutMs: Int64 read FCall_connect_timeout_ms;
    property CallPacketTimeoutMs: Int64 read FCall_packet_timeout_ms;
    property AnimationSearchBotUsername: string read FAnimation_search_bot_username;
    property VenueSearchBotUsername: string read FVenue_search_bot_username;
    property PhotoSearchBotUsername: string read FPhoto_search_bot_username;
    property AuthorizationDate: Int64 read FAuthorization_date;
    property MyId: Int64 read FMy_id;
    property ArchiveAndMuteNewChatsFromUnknownUsers: Boolean read FArchive_and_mute_new_chats_from_unknown_users;
    property IgnoreSensitiveContentRestrictions: Boolean read FIgnore_sensitive_content_restrictions;
    property CanIgnoreSensitiveContentRestrictions: Boolean read FCan_ignore_sensitive_content_restrictions;
    property IsPremiumAvailable: Boolean read FIs_premium_available;
    property AuthenticationToken: string read FAuthentication_token;
  end;

implementation

{ TtgOptions }

end.

