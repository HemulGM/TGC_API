unit TGC.Builder.SendMessage;

interface

uses
  TGC.Classes;

type
  TInputMessageContent = class(TParam);

  TTextEntityType = class(TParam);

  /// <summary>
  /// A bank card number. The getBankCardInfo method can be used to get information about the bank card.
  /// </summary>
  TTextEntityTypeBankCardNumber = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A bold text.
  /// </summary>
  TTextEntityTypeBold = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A bot command, beginning with "/".
  /// </summary>
  TTextEntityTypeBotCommand = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A cashtag text, beginning with "$" and consisting of capital English letters (e.g., "$USD").
  /// </summary>
  TTextEntityTypeCashtag = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Text that must be formatted as if inside a code HTML tag.
  /// </summary>
  TTextEntityTypeCode = class(TTextEntityType)
    constructor Create; reintroduce;
  end;
  /// <summary>
  /// An email address.
  /// </summary>

  TTextEntityTypeEmailAddress = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A hashtag text, beginning with "#".
  /// </summary>
  TTextEntityTypeHashtag = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// An italic text.
  /// </summary>
  TTextEntityTypeItalic = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A media timestamp.
  /// </summary>
  TTextEntityTypeMediaTimestamp = class(TTextEntityType)
    /// <summary>
    /// Timestamp from which a video/audio/video note/voice note playing must start, in seconds. The media can be in the content or the web page preview of the current message, or in the same places in the replied message.
    /// </summary>
    constructor Create(const MediaTimestamp: Integer); reintroduce;
  end;

  /// <summary>
  /// A mention of a user by their username.
  /// </summary>
  TTextEntityTypeMention = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A text shows instead of a raw mention of the user (e.g., when the user has no username).
  /// </summary>
  TTextEntityTypeMentionName = class(TTextEntityType)
    /// <summary>
    /// Identifier of the mentioned user.
    /// </summary>
    constructor Create(const UserId: Int64); reintroduce;
  end;

  /// <summary>
  /// A phone number.
  /// </summary>
  TTextEntityTypePhoneNumber = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Text that must be formatted as if inside a pre HTML tag.
  /// </summary>
  TTextEntityTypePre = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Text that must be formatted as if inside pre, and code HTML tags.
  /// </summary>
  TTextEntityTypePreCode = class(TTextEntityType)
    /// <summary>
    /// Programming language of the code; as defined by the sender.
    /// </summary>
    constructor Create(const Language: string); reintroduce;
  end;

  /// <summary>
  /// A strikethrough text.
  /// </summary>
  TTextEntityTypeStrikethrough = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A text description shown instead of a raw URL.
  /// </summary>
  TTextEntityTypeTextUrl = class(TTextEntityType)
    /// <summary>
    /// HTTP or tg:// URL to be opened when the link is clicked.
    /// </summary>
    constructor Create(const Url: string); reintroduce;
  end;

  /// <summary>
  /// An underlined text.
  /// </summary>
  TTextEntityTypeUnderline = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// An HTTP URL.
  /// </summary>
  TTextEntityTypeUrl = class(TTextEntityType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Represents a part of the text that needs to be formatted in some unusual way.
  /// </summary>
  TTextEntity = class(TParam)
    /// <summary>
    /// Offset of the entity, in UTF-16 code units.
    /// </summary>
    function Offset(const Value: Integer): TTextEntity;
    /// <summary>
    /// Length of the entity, in UTF-16 code units.
    /// </summary>
    function Length(const Value: Integer): TTextEntity;
    /// <summary>
    /// Type of the entity.
    /// </summary>
    function EntityType(const Value: TTextEntityType): TTextEntity;
    constructor Create; reintroduce;
  end;

  TFormattedText = class(TParam)
    /// <summary>
    /// The text.
    /// </summary>
    function Text(const Value: string): TFormattedText;
    /// <summary>
    /// Entities contained in the text. Entities can be nested, but must not mutually
    /// intersect with each other. Pre, Code and PreCode entities can't contain other entities.
    /// Bold, Italic, Underline and Strikethrough entities can contain and to be contained in all other entities.
    /// All other entities can't contain each other.
    /// </summary>
    function Entities(const Value: TArray<TTextEntity>): TFormattedText;
    constructor Create; reintroduce;
  end;

  TInputMessageText = class(TInputMessageContent)
    /// <summary>
    /// True, if rich web page previews for URLs in the message text must be disabled.
    /// </summary>
    function DisableWebPagePreview: TInputMessageText;
    /// <summary>
    /// True, if a chat message draft must be deleted.
    /// </summary>
    function ClearDraft: TInputMessageText;
    /// <summary>
    /// Formatted text to be sent; 1-GetOption("message_text_length_max") characters. Only Bold, Italic, Underline, Strikethrough, Code, Pre, PreCode, TextUrl and MentionName entities are allowed to be specified manually.
    /// </summary>
    function Text(const Value: TFormattedText): TInputMessageText;
    constructor Create; reintroduce;
  end;

  TBuildSendMessage = class(TParam)
    /// <summary>
    /// Target chat.
    /// </summary>
    function ChatId(const Value: Int64): TBuildSendMessage;
    /// <summary>
    /// If not 0, a message thread identifier in which the message will be sent.
    /// </summary>
    function MessageThreadId(const Value: Int64): TBuildSendMessage;
    /// <summary>
    /// Identifier of the message to reply to or 0.
    /// </summary>
    function ReplyToMessageId(const Value: Int64): TBuildSendMessage;
    /// <summary>
    /// The content of the message to be sent.
    /// </summary>
    function InputMessageContent(const Value: TInputMessageContent): TBuildSendMessage;
    constructor Create; reintroduce;
  end;

implementation

uses
  HGM.JSONParams;

{ TBuildSendMessage }

function TBuildSendMessage.ChatId(const Value: Int64): TBuildSendMessage;
begin
  Result := TBuildSendMessage(Add('chat_id', Value));
end;

constructor TBuildSendMessage.Create;
begin
  inherited Create('sendMessage');
end;

function TBuildSendMessage.InputMessageContent(const Value: TInputMessageContent): TBuildSendMessage;
begin
  Result := TBuildSendMessage(Add('input_message_content', Value));
end;

function TBuildSendMessage.MessageThreadId(const Value: Int64): TBuildSendMessage;
begin
  Result := TBuildSendMessage(Add('message_thread_id', Value));
end;

function TBuildSendMessage.ReplyToMessageId(const Value: Int64): TBuildSendMessage;
begin
  Result := TBuildSendMessage(Add('reply_to_message_id', Value));
end;

{ TInputMessageText }

function TInputMessageText.ClearDraft: TInputMessageText;
begin
  Result := TInputMessageText(Add('clear_draft', True));
end;

constructor TInputMessageText.Create;
begin
  inherited Create('inputMessageText');
end;

function TInputMessageText.DisableWebPagePreview: TInputMessageText;
begin
  Result := TInputMessageText(Add('disable_web_page_preview', True));
end;

function TInputMessageText.Text(const Value: TFormattedText): TInputMessageText;
begin
  Result := TInputMessageText(Add('text', Value));
end;

{ TFormattedText }

constructor TFormattedText.Create;
begin
  inherited Create('formattedText');
end;

function TFormattedText.Entities(const Value: TArray<TTextEntity>): TFormattedText;
begin
  Result := TFormattedText(Add('entities', TArray<TJSONParam>(Value)));
end;

function TFormattedText.Text(const Value: string): TFormattedText;
begin
  Result := TFormattedText(Add('text', Value));
end;

{ TTextEntity }

constructor TTextEntity.Create;
begin
  inherited Create('textEntity');
end;

function TTextEntity.EntityType(const Value: TTextEntityType): TTextEntity;
begin
  Result := TTextEntity(Add('type', Value));
end;

function TTextEntity.Length(const Value: Integer): TTextEntity;
begin
  Result := TTextEntity(Add('length', Value));
end;

function TTextEntity.Offset(const Value: Integer): TTextEntity;
begin
  Result := TTextEntity(Add('offset', Value));
end;

{ TTextEntityTypeMediaTimestamp }

constructor TTextEntityTypeMediaTimestamp.Create(const MediaTimestamp: Integer);
begin
  inherited Create('textEntityTypeMediaTimestamp');
  Add('media_timestamp', MediaTimestamp);
end;

{ TTextEntityTypeItalic }

constructor TTextEntityTypeItalic.Create;
begin
  inherited Create('textEntityTypeItalic');
end;

{ TTextEntityTypeHashtag }

constructor TTextEntityTypeHashtag.Create;
begin
  inherited Create('textEntityTypeHashtag');
end;

{ TTextEntityTypeEmailAddress }

constructor TTextEntityTypeEmailAddress.Create;
begin
  inherited Create('textEntityTypeEmailAddress');
end;

{ TTextEntityTypeCode }

constructor TTextEntityTypeCode.Create;
begin
  inherited Create('textEntityTypeCode');
end;

{ TTextEntityTypeCashtag }

constructor TTextEntityTypeCashtag.Create;
begin
  inherited Create('textEntityTypeCashtag');
end;

{ TTextEntityTypeBotCommand }

constructor TTextEntityTypeBotCommand.Create;
begin
  inherited Create('textEntityTypeBotCommand');
end;

{ TTextEntityTypeBold }

constructor TTextEntityTypeBold.Create;
begin
  inherited Create('textEntityTypeBold');
end;

{ TTextEntityTypeBankCardNumber }

constructor TTextEntityTypeBankCardNumber.Create;
begin
  inherited Create('textEntityTypeBankCardNumber');
end;

{ TTextEntityTypeMention }

constructor TTextEntityTypeMention.Create;
begin
  inherited Create('textEntityTypeMention');
end;

{ TTextEntityTypeMentionName }

constructor TTextEntityTypeMentionName.Create(const UserId: Int64);
begin
  inherited Create('textEntityTypeMentionName');
  Add('user_id', UserId);
end;

{ TTextEntityTypePhoneNumber }

constructor TTextEntityTypePhoneNumber.Create;
begin
  inherited Create('textEntityTypePhoneNumber');
end;

{ TTextEntityTypePre }

constructor TTextEntityTypePre.Create;
begin
  inherited Create('textEntityTypePre');
end;

{ TTextEntityTypePreCode }

constructor TTextEntityTypePreCode.Create(const Language: string);
begin
  inherited Create('textEntityTypePreCode');
  Add('language', Language);
end;

{ TTextEntityTypeStrikethrough }

constructor TTextEntityTypeStrikethrough.Create;
begin
  inherited Create('textEntityTypeStrikethrough');
end;

{ TTextEntityTypeTextUrl }

constructor TTextEntityTypeTextUrl.Create(const Url: string);
begin
  inherited Create('textEntityTypeTextUrl');
  Add('url', Url);
end;

{ TTextEntityTypeUnderline }

constructor TTextEntityTypeUnderline.Create;
begin
  inherited Create('textEntityTypeUnderline');
end;

{ TTextEntityTypeUrl }

constructor TTextEntityTypeUrl.Create;
begin
  inherited Create('textEntityTypeUrl');
end;

end.

