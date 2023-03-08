unit TGC.Builder.SendMessage;

interface

uses
  TGC.Classes, System.SysUtils;

type
  /// <summary>
  /// This class is an abstract base class. The content of a message to send.
  /// </summary>
  TInputMessageContent = class(TParam);

  {$REGION 'TInputMessageText'}

  /// <summary>
  /// This class is an abstract base class. Represents a part of the text which must be formatted differently.
  /// </summary>
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

  /// <summary>
  /// A text message.
  /// </summary>
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

  /// <summary>
  /// A text message.
  /// </summary>
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

  {$ENDREGION}

  {$REGION 'TInputFile'}

  TInputFile = class(TParam);

  /// <summary>
  /// A file generated by the application.
  /// </summary>
  TInputFileGenerated = class(TInputFile)
    /// <summary>
    ///	Local path to a file from which the file is generated; may be empty if there is no such file.
    /// </summary>
    function OriginalPath(const Value: string): TInputFileGenerated;
    /// <summary>
    /// String specifying the conversion applied to the original file; must be persistent across application restarts. Conversions beginning with '#' are reserved for internal TDLib usage.
    /// </summary>
    function Conversion(const Value: string): TInputFileGenerated;
    /// <summary>
    /// Expected size of the generated file, in bytes; 0 if unknown.
    /// </summary>
    function ExpectedSize(const Value: Int32): TInputFileGenerated;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A file defined by its unique ID.
  /// </summary>
  TInputFileId = class(TInputFile)
    /// <summary>
    /// Unique file identifier.
    /// </summary>
    function Id(const Value: Int32): TInputFileId;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A file defined by a local path.
  /// </summary>
  TInputFileLocal = class(TInputFile)
    /// <summary>
    /// Local path to the file.
    /// </summary>
    function Path(const Value: string): TInputFileLocal;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A file defined by its remote ID.
  /// The remote ID is guaranteed to be usable only if the corresponding file is
  /// still accessible to the user and known to TDLib. For example, if the file is
  /// from a message, then the message must be not deleted and accessible to the user.
  /// If the file database is disabled, then the corresponding object with the file must be preloaded by the application.
  /// </summary>
  TInputFileRemote = class(TInputFile)
    /// <summary>
    /// Remote file identifier.
    /// </summary>
    function Id(const Value: string): TInputFileRemote;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputThumbnail'}

  /// <summary>
  /// A thumbnail to be sent along with a file; must be in JPEG or WEBP format for stickers, and less than 200 KB in size.
  /// </summary>
  TInputThumbnail = class(TParam)
    /// <summary>
    /// Thumbnail file to send. Sending thumbnails by file_id is currently not supported.
    /// </summary>
    function Thumbnail(const Value: TInputFile): TInputThumbnail;
    /// <summary>
    /// Thumbnail width, usually shouldn't exceed 320. Use 0 if unknown.
    /// </summary>
    function Width(const Value: Int32): TInputThumbnail;
    /// <summary>
    /// Thumbnail height, usually shouldn't exceed 320. Use 0 if unknown.
    /// </summary>
    function Height(const Value: Int32): TInputThumbnail;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageAnimation'}

  /// <summary>
  /// An animation message (GIF-style).
  /// </summary>
  TInputMessageAnimation = class(TInputMessageContent)
    /// <summary>
    /// Animation file to be sent.
    /// </summary>
    function Animation(const Value: TInputFile): TInputMessageAnimation;
    /// <summary>
    /// Animation thumbnail; pass null to skip thumbnail uploading.
    /// </summary>
    function Thumbnail(const Value: TInputThumbnail): TInputMessageAnimation;
    /// <summary>
    /// File identifiers of the stickers added to the animation, if applicable.
    /// </summary>
    function AddedStickerFileIds(const Value: TArray<Int32>): TInputMessageAnimation;
    /// <summary>
    /// Duration of the animation, in seconds.
    /// </summary>
    function Duration(const Value: Int32): TInputMessageAnimation;
    /// <summary>
    /// Width of the animation; may be replaced by the server.
    /// </summary>
    function Width(const Value: Int32): TInputMessageAnimation;
    /// <summary>
    /// Height of the animation; may be replaced by the server.
    /// </summary>
    function Height(const Value: Int32): TInputMessageAnimation;
    /// <summary>
    /// Animation caption; pass null to use an empty caption; 0-GetOption("message_caption_length_max") characters.
    /// </summary>
    function Caption(const Value: TFormattedText): TInputMessageAnimation;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageAudio'}

  /// <summary>
  /// An audio message.
  /// </summary>
  TInputMessageAudio = class(TInputMessageContent)
    /// <summary>
    /// Audio file to be sent.
    /// </summary>
    function Audio(const Value: TInputFile): TInputMessageAudio;
    /// <summary>
    /// Thumbnail of the cover for the album; pass null to skip thumbnail uploading.
    /// </summary>
    function AlbumCoverThumbnail(const Value: TInputThumbnail): TInputMessageAudio;
    /// <summary>
    /// Duration of the audio, in seconds; may be replaced by the server.
    /// </summary>
    function Duration(const Value: Int32): TInputMessageAudio;
    /// <summary>
    /// Title of the audio; 0-64 characters; may be replaced by the server.
    /// </summary>
    function Title(const Value: string): TInputMessageAudio;
    /// <summary>
    /// Performer of the audio; 0-64 characters, may be replaced by the server.
    /// </summary>
    function Performer(const Value: string): TInputMessageAudio;
    /// <summary>
    /// Audio caption; pass null to use an empty caption; 0-GetOption("message_caption_length_max") characters.
    /// </summary>
    function Caption(const Value: TFormattedText): TInputMessageAudio;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TContact'}

  /// <summary>
  /// Describes a user contact.
  /// </summary>
  TContact = class(TParam)
    /// <summary>
    /// Phone number of the user.
    /// </summary>
    function PhoneNumber(const Value: string): TContact;
    /// <summary>
    /// First name of the user; 1-255 characters in length.
    /// </summary>
    function FirstName(const Value: string): TContact;
    /// <summary>
    /// Last name of the user.
    /// </summary>
    function LastName(const Value: string): TContact;
    /// <summary>
    /// Additional data about the user in a form of vCard; 0-2048 bytes in length.
    /// </summary>
    function VCard(const Value: string): TContact;
    /// <summary>
    /// Identifier of the user, if known; otherwise 0.
    /// </summary>
    function UserId(const Value: Int64): TContact;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageContact'}

  /// <summary>
  /// A message containing a user contact.
  /// </summary>
  TInputMessageContact = class(TInputMessageContent)
    /// <summary>
    /// Contact to send.
    /// </summary>
    function Contact(const Value: TContact): TInputMessageContact;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageDice'}

  /// <summary>
  /// A dice message.
  /// </summary>
  TInputMessageDice = class(TInputMessageContent)
    /// <summary>
    /// Emoji on which the dice throw animation is based.
    /// </summary>
    function Emoji(const Value: string): TInputMessageDice;
    /// <summary>
    /// True, if the chat message draft must be deleted.
    /// </summary>
    function ClearDraft(const Value: Boolean): TInputMessageDice;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageDocument'}

  /// <summary>
  /// A document message (general file).
  /// </summary>
  TInputMessageDocument = class(TInputMessageContent)
    /// <summary>
    /// Document to be sent.
    /// </summary>
    function Document(const Value: TInputFile): TInputMessageDocument;
    /// <summary>
    /// Document thumbnail; pass null to skip thumbnail uploading.
    /// </summary>
    function Thumbnail(const Value: TInputThumbnail): TInputMessageDocument;
    /// <summary>
    /// If true, automatic file type detection will be disabled and the document will be always sent as file. Always true for files sent to secret chats.
    /// </summary>
    function DisableContentTypeDetection(const Value: Boolean): TInputMessageDocument;
    /// <summary>
    /// Document caption; pass null to use an empty caption; 0-GetOption("message_caption_length_max") characters.
    /// </summary>
    function Caption(const Value: TFormattedText): TInputMessageDocument;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TMessageCopyOptions'}

  /// <summary>
  /// Options to be used when a message content is copied without reference to the original sender.
  /// Service messages and messageInvoice can't be copied.
  /// </summary>
  TMessageCopyOptions = class(TParam)
    /// <summary>
    /// True, if content of the message needs to be copied without reference to the original sender. Always true if the message is forwarded to a secret chat or is local.
    /// </summary>
    function SendCopy(const Value: Boolean): TMessageCopyOptions;
    /// <summary>
    /// True, if content of the message needs to be copied without reference to the original sender. Always true if the message is forwarded to a secret chat or is local.
    /// </summary>
    function ReplaceCaption(const Value: Boolean): TMessageCopyOptions;
    /// <summary>
    /// New message caption; pass null to copy message without caption. Ignored if replace_caption is false.
    /// </summary>
    function NewCaption(const Value: TFormattedText): TMessageCopyOptions;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageForwarded'}

  /// <summary>
  /// A forwarded message.
  /// </summary>
  TInputMessageForwarded = class(TInputMessageContent)
    /// <summary>
    /// Identifier for the chat this forwarded message came from.
    /// </summary>
    function FromChatId(const Value: Int64): TInputMessageForwarded;
    /// <summary>
    /// Identifier of the message to forward.
    /// </summary>
    function MessageId(const Value: Int64): TInputMessageForwarded;
    /// <summary>
    /// True, if a game message is being shared from a launched game; applies only to game messages.
    /// </summary>
    function InGameShare(const Value: Boolean): TInputMessageForwarded;
    /// <summary>
    /// Options to be used to copy content of the message without reference to the original sender; pass null to forward the message as usual.
    /// </summary>
    function CopyOptions(const Value: TMessageCopyOptions): TInputMessageForwarded;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageGame'}

  /// <summary>
  /// A message with a game; not supported for channels or secret chats.
  /// </summary>
  TInputMessageGame = class(TInputMessageContent)
    /// <summary>
    /// User identifier of the bot that owns the game.
    /// </summary>
    function BotUserId(const Value: Int64): TInputMessageGame;
    /// <summary>
    /// Short name of the game.
    /// </summary>
    function GameShortName(const Value: string): TInputMessageGame;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TLabeledPricePart'}

  /// <summary>
  /// Portion of the price of a product (e.g., "delivery cost", "tax amount").
  /// </summary>
  TLabeledPricePart = class(TParam)
    /// <summary>
    /// Label for this portion of the product price.
    /// </summary>
    function &Label(const Value: string): TLabeledPricePart;
    /// <summary>
    /// Currency amount in the smallest units of the currency.
    /// </summary>
    function Amount(const Value: Int64): TLabeledPricePart;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInvoice'}

  /// <summary>
  /// Product invoice.
  /// </summary>
  TInvoice = class(TParam)
    /// <summary>
    /// ISO 4217 currency code.
    /// </summary>
    function Currency(const Value: string): TInvoice;
    /// <summary>
    /// A list of objects used to calculate the total price of the product.
    /// </summary>
    function PriceParts(const Value: TArray<TLabeledPricePart>): TInvoice;
    /// <summary>
    /// The maximum allowed amount of tip in the smallest units of the currency.
    /// </summary>
    function MaxTipAmount(const Value: Int64): TInvoice;
    /// <summary>
    /// Suggested amounts of tip in the smallest units of the currency.
    /// </summary>
    function SuggestedTipAmounts(const Value: TArray<Int64>): TInvoice;
    /// <summary>
    /// True, if the payment is a test payment.
    /// </summary>
    function IsTest(const Value: Boolean): TInvoice;
    /// <summary>
    /// True, if the user's name is needed for payment.
    /// </summary>
    function NeedName(const Value: Boolean): TInvoice;
    /// <summary>
    /// True, if the user's phone number is needed for payment.
    /// </summary>
    function NeedPhoneNumber(const Value: Boolean): TInvoice;
    /// <summary>
    /// True, if the user's email address is needed for payment.
    /// </summary>
    function NeedEmailAddress(const Value: Boolean): TInvoice;
    /// <summary>
    /// True, if the user's shipping address is needed for payment.
    /// </summary>
    function NeedShippingAddress(const Value: Boolean): TInvoice;
    /// <summary>
    /// True, if the user's phone number will be sent to the provider.
    /// </summary>
    function SendPhoneNumberToProvider(const Value: Boolean): TInvoice;
    /// <summary>
    /// True, if the user's email address will be sent to the provider.
    /// </summary>
    function SendEmailAddressToProvider(const Value: Boolean): TInvoice;
    /// <summary>
    /// True, if the total price depends on the shipping method.
    /// </summary>
    function IsFlexible(const Value: Boolean): TInvoice;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageInvoice'}

  /// <summary>
  /// A message with an invoice; can be used only by bots.
  /// </summary>
  TInputMessageInvoice = class(TInputMessageContent)
    /// <summary>
    /// Invoice.
    /// </summary>
    function Invoice(const Value: Int64): TInputMessageInvoice;
    /// <summary>
    /// Product title; 1-32 characters.
    /// </summary>
    function Title(const Value: string): TInputMessageInvoice;
    /// <summary>
    /// Product description; 0-255 characters.
    /// </summary>
    function Description(const Value: string): TInputMessageInvoice;
    /// <summary>
    /// Product photo URL; optional.
    /// </summary>
    function PhotoUrl(const Value: string): TInputMessageInvoice;
    /// <summary>
    /// Product photo size.
    /// </summary>
    function PhotoSize(const Value: Int32): TInputMessageInvoice;
    /// <summary>
    /// Product photo width.
    /// </summary>
    function PhotoWidth(const Value: Int32): TInputMessageInvoice;
    /// <summary>
    /// Product photo height.
    /// </summary>
    function PhotoHeight(const Value: Int32): TInputMessageInvoice;
    /// <summary>
    /// The invoice payload.
    /// </summary>
    function Payload(const Value: TBytes): TInputMessageInvoice;
    /// <summary>
    /// Payment provider token.
    /// </summary>
    function ProviderToken(const Value: string): TInputMessageInvoice;
    /// <summary>
    /// JSON-encoded data about the invoice, which will be shared with the payment provider.
    /// </summary>
    function ProviderData(const Value: string): TInputMessageInvoice;
    /// <summary>
    /// Unique invoice bot deep link parameter for the generation of this invoice. If empty, it would be possible to pay directly from forwards of the invoice message.
    /// </summary>
    function StartParameter(const Value: string): TInputMessageInvoice;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TLocation'}

  /// <summary>
  /// Describes a location on planet Earth.
  /// </summary>
  TLocation = class(TParam)
    /// <summary>
    /// Latitude of the location in degrees; as defined by the sender.
    /// </summary>
    function Latitude(const Value: Double): TLocation;
    /// <summary>
    /// Longitude of the location, in degrees; as defined by the sender.
    /// </summary>
    function Longitude(const Value: Double): TLocation;
    /// <summary>
    /// The estimated horizontal accuracy of the location, in meters; as defined by the sender. 0 if unknown.
    /// </summary>
    function HorizontalAccuracy(const Value: Double): TLocation;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageLocation'}

  /// <summary>
  /// A message with a location.
  /// </summary>
  TInputMessageLocation = class(TInputMessageContent)
    /// <summary>
    /// Location to be sent.
    /// </summary>
    function Location(const Value: TLocation): TInputMessageLocation;
    /// <summary>
    /// Period for which the location can be updated, in seconds; must be between 60 and 86400 for a live location and 0 otherwise.
    /// </summary>
    function LivePeriod(const Value: Int32): TInputMessageLocation;
    /// <summary>
    /// For live locations, a direction in which the location moves, in degrees; 1-360. Pass 0 if unknown.
    /// </summary>
    function Heading(const Value: Int32): TInputMessageLocation;
    /// <summary>
    /// For live locations, a maximum distance to another chat member for proximity alerts, in meters (0-100000). Pass 0 if the notification is disabled. Can't be enabled in channels and Saved Messages.
    /// </summary>
    function ProximityAlertRadius(const Value: Int32): TInputMessageLocation;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessagePhoto'}

  /// <summary>
  /// A photo message.
  /// </summary>
  TInputMessagePhoto = class(TInputMessageContent)
    /// <summary>
    /// Photo to send.
    /// </summary>
    function Photo(const Value: TInputFile): TInputMessagePhoto;
    /// <summary>
    /// Photo thumbnail to be sent; pass null to skip thumbnail uploading. The thumbnail is sent to the other party only in secret chats.
    /// </summary>
    function Thumbnail(const Value: TInputThumbnail): TInputMessagePhoto;
    /// <summary>
    /// File identifiers of the stickers added to the photo, if applicable.
    /// </summary>
    function AddedStickerFileIds(const Value: TArray<Int32>): TInputMessagePhoto;
    /// <summary>
    /// Photo width.
    /// </summary>
    function Width(const Value: Int32): TInputMessagePhoto;
    /// <summary>
    /// Photo height.
    /// </summary>
    function Height(const Value: Int32): TInputMessagePhoto;
    /// <summary>
    /// Photo caption; pass null to use an empty caption; 0-GetOption("message_caption_length_max") characters.
    /// </summary>
    function Caption(const Value: TFormattedText): TInputMessagePhoto;
    /// <summary>
    /// Photo TTL (Time To Live), in seconds (0-60). A non-zero TTL can be specified only in private chats.
    /// </summary>
    function TTL(const Value: Int32): TInputMessagePhoto;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TPollType'}

  /// <summary>
  /// This class is an abstract base class. Describes the type of a poll.
  /// </summary>
  TPollType = class(TParam);

  /// <summary>
  /// A poll in quiz mode, which has exactly one correct answer option and can be answered only once.
  /// </summary>
  TPollTypeQuiz = class(TPollType)
    /// <summary>
    /// 0-based identifier of the correct answer option; -1 for a yet unanswered poll.
    /// </summary>
    function CorrectOptionId(const Value: Int32): TPollTypeQuiz;
    /// <summary>
    /// Text that is shown when the user chooses an incorrect answer or taps on the lamp icon; 0-200 characters with at most 2 line feeds; empty for a yet unanswered poll.
    /// </summary>
    function Explanation(const Value: TFormattedText): TPollTypeQuiz;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A regular poll.
  /// </summary>
  TPollTypeRegular = class(TPollType)
    /// <summary>
    /// True, if multiple answer options can be chosen simultaneously.
    /// </summary>
    function AllowMultipleAnswers(const Value: Boolean): TPollTypeRegular;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessagePoll'}

  /// <summary>
  /// A message with a poll. Polls can't be sent to secret chats. Polls can be sent only to a private chat with a bot.
  /// </summary>
  TInputMessagePoll = class(TInputMessageContent)
    /// <summary>
    /// Poll question; 1-255 characters (up to 300 characters for bots).
    /// </summary>
    function Question(const Value: string): TInputMessagePoll;
    /// <summary>
    /// List of poll answer options, 2-10 strings 1-100 characters each.
    /// </summary>
    function Options(const Value: TArray<string>): TInputMessagePoll;
    /// <summary>
    /// True, if the poll voters are anonymous. Non-anonymous polls can't be sent or forwarded to channels.
    /// </summary>
    function IsAnonymous(const Value: Boolean): TInputMessagePoll;
    /// <summary>
    /// Type of the poll.
    /// </summary>
    function &Type(const Value: TPollType): TInputMessagePoll;
    /// <summary>
    /// Amount of time the poll will be active after creation, in seconds; for bots only.
    /// </summary>
    function OpenPeriod(const Value: Int32): TInputMessagePoll;
    /// <summary>
    /// Point in time (Unix timestamp) when the poll will automatically be closed; for bots only.
    /// </summary>
    function CloseDate(const Value: TDateTime): TInputMessagePoll;
    /// <summary>
    /// True, if the poll needs to be sent already closed; for bots only.
    /// </summary>
    function IsClosed(const Value: Boolean): TInputMessagePoll;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageSticker'}

  /// <summary>
  /// A sticker message.
  /// </summary>
  TInputMessageSticker = class(TInputMessageContent)
    /// <summary>
    /// Sticker to be sent.
    /// </summary>
    function Sticker(const Value: TInputFile): TInputMessageSticker;
    /// <summary>
    /// Sticker thumbnail; pass null to skip thumbnail uploading.
    /// </summary>
    function Thumbnail(const Value: TInputThumbnail): TInputMessageSticker;
    /// <summary>
    /// Sticker width.
    /// </summary>
    function Width(const Value: Int32): TInputMessageSticker;
    /// <summary>
    /// Sticker height.
    /// </summary>
    function Height(const Value: Int32): TInputMessageSticker;
    /// <summary>
    /// Emoji used to choose the sticker.
    /// </summary>
    function Emoji(const Value: string): TInputMessageSticker;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TVenue'}

  /// <summary>
  /// Describes a venue.
  /// </summary>
  TVenue = class(TParam)
    /// <summary>
    /// Venue location; as defined by the sender.
    /// </summary>
    function Location(const Value: TLocation): TVenue;
    /// <summary>
    /// Venue name; as defined by the sender.
    /// </summary>
    function Title(const Value: string): TVenue;
    /// <summary>
    /// Venue address; as defined by the sender.
    /// </summary>
    function Address(const Value: string): TVenue;
    /// <summary>
    /// Provider of the venue database; as defined by the sender. Currently, only "foursquare" and "gplaces" (Google Places) need to be supported.
    /// </summary>
    function Provider(const Value: string): TVenue;
    /// <summary>
    /// Identifier of the venue in the provider database; as defined by the sender.
    /// </summary>
    function Id(const Value: string): TVenue;
    /// <summary>
    /// Type of the venue in the provider database; as defined by the sender.
    /// </summary>
    function &Type(const Value: string): TVenue;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageVenue'}

  /// <summary>
  /// A message with information about a venue.
  /// </summary>
  TInputMessageVenue = class(TInputMessageContent)
    /// <summary>
    /// Venue to send.
    /// </summary>
    function Venue(const Value: TVenue): TInputMessageVenue;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageVideo'}

  /// <summary>
  /// A video message.
  /// </summary>
  TInputMessageVideo = class(TInputMessageContent)
    /// <summary>
    /// Video to be sent.
    /// </summary>
    function Video(const Value: TInputFile): TInputMessageVideo;
    /// <summary>
    /// Video thumbnail; pass null to skip thumbnail uploading.
    /// </summary>
    function Thumbnail(const Value: TInputThumbnail): TInputMessageVideo;
    /// <summary>
    /// File identifiers of the stickers added to the video, if applicable.
    /// </summary>
    function AddedStickerFileIds(const Value: TArray<Int32>): TInputMessageVideo;
    /// <summary>
    /// Duration of the video, in seconds.
    /// </summary>
    function Duration(const Value: Int32): TInputMessageVideo;
    /// <summary>
    /// Video width.
    /// </summary>
    function Width(const Value: Int32): TInputMessageVideo;
    /// <summary>
    /// Video height.
    /// </summary>
    function Height(const Value: Int32): TInputMessageVideo;
    /// <summary>
    /// True, if the video is supposed to be streamed.
    /// </summary>
    function SupportsStreaming(const Value: Boolean): TInputMessageVideo;
    /// <summary>
    /// Video caption; pass null to use an empty caption; 0-GetOption("message_caption_length_max") characters.
    /// </summary>
    function Caption(const Value: TFormattedText): TInputMessageVideo;
    /// <summary>
    /// Video TTL (Time To Live), in seconds (0-60). A non-zero TTL can be specified only in private chats.
    /// </summary>
    function TTL(const Value: Int32): TInputMessageVideo;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageVideoNote'}

  /// <summary>
  /// A video note message.
  /// </summary>
  TInputMessageVideoNote = class(TInputMessageContent)
    /// <summary>
    /// Video note to be sent.
    /// </summary>
    function VideoNote(const Value: TInputFile): TInputMessageVideoNote;
    /// <summary>
    /// Video thumbnail; pass null to skip thumbnail uploading.
    /// </summary>
    function Thumbnail(const Value: TInputThumbnail): TInputMessageVideoNote;
    /// <summary>
    /// Duration of the video, in seconds.
    /// </summary>
    function Duration(const Value: Int32): TInputMessageVideoNote;
    /// <summary>
    /// Video width and height; must be positive and not greater than 640.
    /// </summary>
    function Length(const Value: Int32): TInputMessageVideoNote;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInputMessageVoiceNote'}

  /// <summary>
  /// A voice note message.
  /// </summary>
  TInputMessageVoiceNote = class(TInputMessageContent)
    /// <summary>
    /// Voice note to be sent.
    /// </summary>
    function VoiceNote(const Value: TInputFile): TInputMessageVoiceNote;
    /// <summary>
    /// Duration of the voice note, in seconds.
    /// </summary>
    function Duration(const Value: Int32): TInputMessageVoiceNote;
    /// <summary>
    /// Waveform representation of the voice note, in 5-bit format.
    /// </summary>
    function Waveform(const Value: TBytes): TInputMessageVoiceNote;
    /// <summary>
    /// Voice note caption; pass null to use an empty caption; 0-GetOption("message_caption_length_max") characters.
    /// </summary>
    function Caption(const Value: TFormattedText): TInputMessageVoiceNote;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TMessageSchedulingState'}

  /// <summary>
  /// This class is an abstract base class. Contains information about the time when a scheduled message will be sent.
  /// </summary>
  TMessageSchedulingState = class(TParam);

  /// <summary>
  /// The message will be sent at the specified date.
  /// </summary>
  TMessageSchedulingStateSendAtDate = class(TMessageSchedulingState)
    /// <summary>
    /// Date the message will be sent. The date must be within 367 days in the future.
    /// </summary>
    function SendDate(const Value: TDateTime): TMessageSchedulingStateSendAtDate;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// The message will be sent when the peer will be online. Applicable to private chats only and when the exact online status of the peer is known.
  /// </summary>
  TMessageSchedulingStateSendWhenOnline = class(TMessageSchedulingState)
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TMessageSendOptions'}

  /// <summary>
  /// Options to be used when a message is sent.
  /// </summary>
  TMessageSendOptions = class(TParam)
    /// <summary>
    /// Pass true to disable notification for the message.
    /// </summary>
    function DisableNotification(const Value: Boolean): TMessageSendOptions;
    /// <summary>
    /// Pass true if the message is sent from the background.
    /// </summary>
    function FromBackground(const Value: Boolean): TMessageSendOptions;
    /// <summary>
    /// Message scheduling state; pass null to send message immediately. Messages sent to a secret chat, live location messages and self-destructing messages can't be scheduled.
    /// </summary>
    function SchedulingState(const Value: TMessageSchedulingState): TMessageSendOptions;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TReplyMarkup'}

  /// <summary>
  /// This class is an abstract base class. Contains a description of a custom keyboard and actions that can be done with it to quickly reply to bots.
  /// </summary>
  TReplyMarkup = class(TParam);

  /// <summary>
  /// Instructs application to force a reply to this message.
  /// </summary>
  TReplyMarkupForceReply = class(TReplyMarkup)
    /// <summary>
    /// True, if a forced reply must automatically be shown to the current user. For outgoing messages, specify true to show the forced reply only for the mentioned users and for the target user of a reply.
    /// </summary>
    function IsPersonal(const Value: Boolean): TReplyMarkupForceReply;
    /// <summary>
    /// If non-empty, the placeholder to be shown in the input field when the reply is active; 0-64 characters.
    /// </summary>
    function InputFieldPlaceholder(const Value: string): TReplyMarkupForceReply;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInlineKeyboardButtonType'}

  /// <summary>
  /// This class is an abstract base class. Describes the type of an inline keyboard button.
  /// </summary>
  TInlineKeyboardButtonType = class(TParam);

  /// <summary>
  /// A button to buy something. This button must be in the first column and row of the keyboard and can be attached only to a message with content of the type messageInvoice.
  /// </summary>
  TInlineKeyboardButtonTypeBuy = class(TInlineKeyboardButtonType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A button that sends a callback query to a bot.
  /// </summary>
  TInlineKeyboardButtonTypeCallback = class(TInlineKeyboardButtonType)
    /// <summary>
    /// Data to be sent to the bot via a callback query.
    /// </summary>
    function Data(const Value: TBytes): TInlineKeyboardButtonTypeCallback;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A button with a game that sends a callback query to a bot. This button must be in the first column and row of the keyboard and can be attached only to a message with content of the type messageGame.
  /// </summary>
  TInlineKeyboardButtonTypeCallbackGame = class(TInlineKeyboardButtonType)
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A button that asks for password of the current user and then sends a callback query to a bot.
  /// </summary>
  TInlineKeyboardButtonTypeCallbackWithPassword = class(TInlineKeyboardButtonType)
    /// <summary>
    /// Data to be sent to the bot via a callback query.
    /// </summary>
    function Data(const Value: TBytes): TInlineKeyboardButtonTypeCallbackWithPassword;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A button that opens a specified URL and automatically authorize the current user if allowed to do so.
  /// </summary>
  TInlineKeyboardButtonTypeLoginUrl = class(TInlineKeyboardButtonType)
    /// <summary>
    /// An HTTP URL to open.
    /// </summary>
    function Url(const Value: string): TInlineKeyboardButtonTypeLoginUrl;
    /// <summary>
    /// Unique button identifier.
    /// </summary>
    function Id(const Value: Int64): TInlineKeyboardButtonTypeLoginUrl;
    /// <summary>
    /// If non-empty, new text of the button in forwarded messages.
    /// </summary>
    function ForwardText(const Value: string): TInlineKeyboardButtonTypeLoginUrl;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A button that forces an inline query to the bot to be inserted in the input field.
  /// </summary>
  TInlineKeyboardButtonTypeSwitchInline = class(TInlineKeyboardButtonType)
    /// <summary>
    /// Inline query to be sent to the bot.
    /// </summary>
    function Query(const Value: string): TInlineKeyboardButtonTypeSwitchInline;
    /// <summary>
    /// True, if the inline query must be sent from the current chat.
    /// </summary>
    function InCurrentChat(const Value: Boolean): TInlineKeyboardButtonTypeSwitchInline;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A button that opens a specified URL.
  /// </summary>
  TInlineKeyboardButtonTypeUrl = class(TInlineKeyboardButtonType)
    /// <summary>
    /// HTTP or tg:// URL to open.
    /// </summary>
    function Url(const Value: string): TInlineKeyboardButtonTypeUrl;
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// A button with a user reference to be handled in the same way as textEntityTypeMentionName entities.
  /// </summary>
  TInlineKeyboardButtonTypeUser = class(TInlineKeyboardButtonType)
    /// <summary>
    /// User identifier.
    /// </summary>
    function UserId(const Value: Int64): TInlineKeyboardButtonTypeUser;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TInlineKeyboardButton'}

  /// <summary>
  /// Represents a single button in an inline keyboard.
  /// </summary>
  TInlineKeyboardButton = class(TParam)
    /// <summary>
    /// Text of the button.
    /// </summary>
    function Text(const Value: string): TInlineKeyboardButton;
    /// <summary>
    /// Type of the button.
    /// </summary>
    function &Type(const Value: TInlineKeyboardButtonType): TInlineKeyboardButton;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  {$REGION 'TReplyMarkupInlineKeyboard'}

  /// <summary>
  /// Contains an inline keyboard layout.
  /// </summary>
  TReplyMarkupInlineKeyboard = class(TReplyMarkup)
    /// <summary>
    /// A list of rows of inline keyboard buttons.
    /// </summary>
    function Rows(const Value: TArray<TArray<TInlineKeyboardButton>>): TReplyMarkupInlineKeyboard;
    constructor Create; reintroduce;
  end;

  {$ENDREGION}

  /// <summary>
  /// Sends a message. Returns the sent message.
  /// </summary>
  TSendMessage = class(TParam)
    /// <summary>
    /// Target chat.
    /// </summary>
    function ChatId(const Value: Int64): TSendMessage;
    /// <summary>
    /// If not 0, a message thread identifier in which the message will be sent.
    /// </summary>
    function MessageThreadId(const Value: Int64): TSendMessage;
    /// <summary>
    /// Identifier of the message to reply to or 0.
    /// </summary>
    function ReplyToMessageId(const Value: Int64): TSendMessage;
    /// <summary>
    /// The content of the message to be sent.
    /// </summary>
    function InputMessageContent(const Value: TInputMessageContent): TSendMessage;
    /// <summary>
    /// Options to be used to send the message; pass null to use default options.
    /// </summary>
    function Options(const Value: TMessageSendOptions): TSendMessage;
    /// <summary>
    /// Markup for replying to the message; pass null if none; for bots only.
    /// </summary>
    function ReplyMarkup(const Value: TReplyMarkup): TSendMessage;
    constructor Create; reintroduce;
  end;

implementation

uses
  HGM.JSONParams;

{ TSendMessage }

function TSendMessage.ChatId(const Value: Int64): TSendMessage;
begin
  Result := TSendMessage(Add('chat_id', Value));
end;

constructor TSendMessage.Create;
begin
  inherited Create('sendMessage');
end;

function TSendMessage.InputMessageContent(const Value: TInputMessageContent): TSendMessage;
begin
  Result := TSendMessage(Add('input_message_content', Value));
end;

function TSendMessage.MessageThreadId(const Value: Int64): TSendMessage;
begin
  Result := TSendMessage(Add('message_thread_id', Value));
end;

function TSendMessage.Options(const Value: TMessageSendOptions): TSendMessage;
begin
  Result := TSendMessage(Add('options', Value));
end;

function TSendMessage.ReplyMarkup(const Value: TReplyMarkup): TSendMessage;
begin
  Result := TSendMessage(Add('reply_markup', Value));
end;

function TSendMessage.ReplyToMessageId(const Value: Int64): TSendMessage;
begin
  Result := TSendMessage(Add('reply_to_message_id', Value));
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


{$REGION 'TFormatedText'}

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

{$ENDREGION}

{ TInputMessageAnimation }

constructor TInputMessageAnimation.Create;
begin
  inherited Create('inputMessageAnimation');
end;

function TInputMessageAnimation.AddedStickerFileIds(const Value: TArray<Int32>): TInputMessageAnimation;
begin
  Result := TInputMessageAnimation(Add('added_sticker_file_ids', Value));
end;

function TInputMessageAnimation.Animation(const Value: TInputFile): TInputMessageAnimation;
begin
  Result := TInputMessageAnimation(Add('animation', Value));
end;

function TInputMessageAnimation.Caption(const Value: TFormattedText): TInputMessageAnimation;
begin
  Result := TInputMessageAnimation(Add('caption', Value));
end;

function TInputMessageAnimation.Duration(const Value: Int32): TInputMessageAnimation;
begin
  Result := TInputMessageAnimation(Add('duration', Value));
end;

function TInputMessageAnimation.Height(const Value: Int32): TInputMessageAnimation;
begin
  Result := TInputMessageAnimation(Add('height', Value));
end;

function TInputMessageAnimation.Thumbnail(const Value: TInputThumbnail): TInputMessageAnimation;
begin
  Result := TInputMessageAnimation(Add('thumbnail', Value));
end;

function TInputMessageAnimation.Width(const Value: Int32): TInputMessageAnimation;
begin
  Result := TInputMessageAnimation(Add('width', Value));
end;

{ TInputFileGenerated }

function TInputFileGenerated.Conversion(const Value: string): TInputFileGenerated;
begin
  Result := TInputFileGenerated(Add('conversion', Value));
end;

constructor TInputFileGenerated.Create;
begin
  inherited Create('inputFileGenerated');
end;

function TInputFileGenerated.ExpectedSize(const Value: Int32): TInputFileGenerated;
begin
  Result := TInputFileGenerated(Add('expected_size', Value));
end;

function TInputFileGenerated.OriginalPath(const Value: string): TInputFileGenerated;
begin
  Result := TInputFileGenerated(Add('original_path', Value));
end;

{ TInputFileId }

constructor TInputFileId.Create;
begin
  inherited Create('inputFileId');
end;

function TInputFileId.Id(const Value: Int32): TInputFileId;
begin
  Result := TInputFileId(Add('id', Value));
end;

{ TInputFileLocal }

constructor TInputFileLocal.Create;
begin
  inherited Create('inputFileLocal');
end;

function TInputFileLocal.Path(const Value: string): TInputFileLocal;
begin
  Result := TInputFileLocal(Add('path', Value));
end;

{ TInputFileRemote }

constructor TInputFileRemote.Create;
begin
  inherited Create('inputFileRemote');
end;

function TInputFileRemote.Id(const Value: string): TInputFileRemote;
begin
  Result := TInputFileRemote(Add('id', Value));
end;

{ TInputThumbnail }

constructor TInputThumbnail.Create;
begin
  inherited Create('inputThumbnail');
end;

function TInputThumbnail.Height(const Value: Int32): TInputThumbnail;
begin
  Result := TInputThumbnail(Add('height', Value));
end;

function TInputThumbnail.Thumbnail(const Value: TInputFile): TInputThumbnail;
begin
  Result := TInputThumbnail(Add('thumbnail', Value));
end;

function TInputThumbnail.Width(const Value: Int32): TInputThumbnail;
begin
  Result := TInputThumbnail(Add('width', Value));
end;

{ TInputMessageAudio }

constructor TInputMessageAudio.Create;
begin
  inherited Create('inputMessageAudio');
end;

function TInputMessageAudio.AlbumCoverThumbnail(const Value: TInputThumbnail): TInputMessageAudio;
begin
  Result := TInputMessageAudio(Add('album_cover_thumbnail', Value));
end;

function TInputMessageAudio.Audio(const Value: TInputFile): TInputMessageAudio;
begin
  Result := TInputMessageAudio(Add('audio', Value));
end;

function TInputMessageAudio.Caption(const Value: TFormattedText): TInputMessageAudio;
begin
  Result := TInputMessageAudio(Add('caption', Value));
end;

function TInputMessageAudio.Duration(const Value: Int32): TInputMessageAudio;
begin
  Result := TInputMessageAudio(Add('duration', Value));
end;

function TInputMessageAudio.Performer(const Value: string): TInputMessageAudio;
begin
  Result := TInputMessageAudio(Add('performer', Value));
end;

function TInputMessageAudio.Title(const Value: string): TInputMessageAudio;
begin
  Result := TInputMessageAudio(Add('title', Value));
end;

{ TContact }

constructor TContact.Create;
begin
  inherited Create('contact');
end;

function TContact.FirstName(const Value: string): TContact;
begin
  Result := TContact(Add('first_name', Value));
end;

function TContact.LastName(const Value: string): TContact;
begin
  Result := TContact(Add('last_name', Value));
end;

function TContact.PhoneNumber(const Value: string): TContact;
begin
  Result := TContact(Add('phone_number', Value));
end;

function TContact.UserId(const Value: Int64): TContact;
begin
  Result := TContact(Add('user_id', Value));
end;

function TContact.VCard(const Value: string): TContact;
begin
  Result := TContact(Add('vcard', Value));
end;

{ TInputMessageContact }

function TInputMessageContact.Contact(const Value: TContact): TInputMessageContact;
begin
  Result := TInputMessageContact(Add('contact', Value));
end;

constructor TInputMessageContact.Create;
begin
  inherited Create('inputMessageContact');
end;

{ TInputMessageDice }

function TInputMessageDice.ClearDraft(const Value: Boolean): TInputMessageDice;
begin
  Result := TInputMessageDice(Add('clear_draft', Value));
end;

constructor TInputMessageDice.Create;
begin
  inherited Create('inputMessageDice');
end;

function TInputMessageDice.Emoji(const Value: string): TInputMessageDice;
begin
  Result := TInputMessageDice(Add('emoji', Value));
end;

{ TInputMessageDocument }

function TInputMessageDocument.Caption(const Value: TFormattedText): TInputMessageDocument;
begin
  Result := TInputMessageDocument(Add('caption', Value));
end;

constructor TInputMessageDocument.Create;
begin
  inherited Create('inputMessageDocument');
end;

function TInputMessageDocument.DisableContentTypeDetection(const Value: Boolean): TInputMessageDocument;
begin
  Result := TInputMessageDocument(Add('disable_content_type_detection', Value));
end;

function TInputMessageDocument.Document(const Value: TInputFile): TInputMessageDocument;
begin
  Result := TInputMessageDocument(Add('document', Value));
end;

function TInputMessageDocument.Thumbnail(const Value: TInputThumbnail): TInputMessageDocument;
begin
  Result := TInputMessageDocument(Add('thumbnail', Value));
end;

{ TInputMessageForwarded }

function TInputMessageForwarded.CopyOptions(const Value: TMessageCopyOptions): TInputMessageForwarded;
begin
  Result := TInputMessageForwarded(Add('copy_options', Value));
end;

constructor TInputMessageForwarded.Create;
begin
  inherited Create('inputMessageForwarded');
end;

function TInputMessageForwarded.FromChatId(const Value: Int64): TInputMessageForwarded;
begin
  Result := TInputMessageForwarded(Add('from_chat_id', Value));
end;

function TInputMessageForwarded.InGameShare(const Value: Boolean): TInputMessageForwarded;
begin
  Result := TInputMessageForwarded(Add('in_game_share', Value));
end;

function TInputMessageForwarded.MessageId(const Value: Int64): TInputMessageForwarded;
begin
  Result := TInputMessageForwarded(Add('message_id', Value));
end;

{ TMessageCopyOptions }

constructor TMessageCopyOptions.Create;
begin
  inherited Create('messageCopyOptions');
end;

function TMessageCopyOptions.NewCaption(const Value: TFormattedText): TMessageCopyOptions;
begin
  Result := TMessageCopyOptions(Add('new_caption', Value));
end;

function TMessageCopyOptions.ReplaceCaption(const Value: Boolean): TMessageCopyOptions;
begin
  Result := TMessageCopyOptions(Add('replace_caption', Value));
end;

function TMessageCopyOptions.SendCopy(const Value: Boolean): TMessageCopyOptions;
begin
  Result := TMessageCopyOptions(Add('send_copy', Value));
end;

{ TInputMessageGame }

function TInputMessageGame.BotUserId(const Value: Int64): TInputMessageGame;
begin
  Result := TInputMessageGame(Add('bot_user_id', Value));
end;

constructor TInputMessageGame.Create;
begin
  inherited Create('inputMessageGame');
end;

function TInputMessageGame.GameShortName(const Value: string): TInputMessageGame;
begin
  Result := TInputMessageGame(Add('game_short_name', Value));
end;

{ TLabeledPricePart }

function TLabeledPricePart.Amount(const Value: Int64): TLabeledPricePart;
begin
  Result := TLabeledPricePart(Add('amount', Value));
end;

constructor TLabeledPricePart.Create;
begin
  inherited Create('labeledPricePart');
end;

function TLabeledPricePart.&Label(const Value: string): TLabeledPricePart;
begin
  Result := TLabeledPricePart(Add('label', Value));
end;

{ TInvoice }

constructor TInvoice.Create;
begin
  inherited Create('invoice');
end;

function TInvoice.Currency(const Value: string): TInvoice;
begin
  Result := TInvoice(Add('currency', Value));
end;

function TInvoice.IsFlexible(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('is_flexible', Value));
end;

function TInvoice.IsTest(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('is_test', Value));
end;

function TInvoice.MaxTipAmount(const Value: Int64): TInvoice;
begin
  Result := TInvoice(Add('max_tip_amount', Value));
end;

function TInvoice.NeedEmailAddress(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('need_email_address', Value));
end;

function TInvoice.NeedName(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('need_name', Value));
end;

function TInvoice.NeedPhoneNumber(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('need_phone_number', Value));
end;

function TInvoice.NeedShippingAddress(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('need_shipping_address', Value));
end;

function TInvoice.PriceParts(const Value: TArray<TLabeledPricePart>): TInvoice;
begin
  Result := TInvoice(Add('price_parts', TArray<TJSONParam>(Value)));
end;

function TInvoice.SendEmailAddressToProvider(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('send_email_address_to_provider', Value));
end;

function TInvoice.SendPhoneNumberToProvider(const Value: Boolean): TInvoice;
begin
  Result := TInvoice(Add('send_phone_number_to_provider', Value));
end;

function TInvoice.SuggestedTipAmounts(const Value: TArray<Int64>): TInvoice;
begin
  Result := TInvoice(Add('suggested_tip_amounts', Value));
end;

{ TInputMessageInvoice }

constructor TInputMessageInvoice.Create;
begin
  inherited Create('inputMessageInvoice');
end;

function TInputMessageInvoice.Description(const Value: string): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('description', Value));
end;

function TInputMessageInvoice.Invoice(const Value: Int64): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('invoice', Value));
end;

function TInputMessageInvoice.Payload(const Value: TBytes): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('payload', Value));
end;

function TInputMessageInvoice.PhotoHeight(const Value: Int32): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('photo_height', Value));
end;

function TInputMessageInvoice.PhotoSize(const Value: Int32): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('photo_size', Value));
end;

function TInputMessageInvoice.PhotoUrl(const Value: string): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('photo_url', Value));
end;

function TInputMessageInvoice.PhotoWidth(const Value: Int32): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('photo_width', Value));
end;

function TInputMessageInvoice.ProviderData(const Value: string): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('provider_data', Value));
end;

function TInputMessageInvoice.ProviderToken(const Value: string): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('provider_token', Value));
end;

function TInputMessageInvoice.StartParameter(const Value: string): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('start_parameter', Value));
end;

function TInputMessageInvoice.Title(const Value: string): TInputMessageInvoice;
begin
  Result := TInputMessageInvoice(Add('title', Value));
end;

{ TLocation }

constructor TLocation.Create;
begin
  inherited Create('location');
end;

function TLocation.HorizontalAccuracy(const Value: Double): TLocation;
begin
  Result := TLocation(Add('horizontal_accuracy', Value));
end;

function TLocation.Latitude(const Value: Double): TLocation;
begin
  Result := TLocation(Add('latitude', Value));
end;

function TLocation.Longitude(const Value: Double): TLocation;
begin
  Result := TLocation(Add('longitude', Value));
end;

{ TInputMessageLocation }

constructor TInputMessageLocation.Create;
begin
  inherited Create('inputMessageLocation');
end;

function TInputMessageLocation.Heading(const Value: Int32): TInputMessageLocation;
begin
  Result := TInputMessageLocation(Add('heading', Value));
end;

function TInputMessageLocation.LivePeriod(const Value: Int32): TInputMessageLocation;
begin
  Result := TInputMessageLocation(Add('live_period', Value));
end;

function TInputMessageLocation.Location(const Value: TLocation): TInputMessageLocation;
begin
  Result := TInputMessageLocation(Add('location', Value));
end;

function TInputMessageLocation.ProximityAlertRadius(const Value: Int32): TInputMessageLocation;
begin
  Result := TInputMessageLocation(Add('proximity_alert_radius', Value));
end;

{ TInputMessagePhoto }

function TInputMessagePhoto.AddedStickerFileIds(const Value: TArray<Int32>): TInputMessagePhoto;
begin
  Result := TInputMessagePhoto(Add('added_sticker_file_ids', Value));
end;

function TInputMessagePhoto.Caption(const Value: TFormattedText): TInputMessagePhoto;
begin
  Result := TInputMessagePhoto(Add('caption', Value));
end;

constructor TInputMessagePhoto.Create;
begin
  inherited Create('inputMessagePhoto');
end;

function TInputMessagePhoto.Height(const Value: Int32): TInputMessagePhoto;
begin
  Result := TInputMessagePhoto(Add('height', Value));
end;

function TInputMessagePhoto.Photo(const Value: TInputFile): TInputMessagePhoto;
begin
  Result := TInputMessagePhoto(Add('photo', Value));
end;

function TInputMessagePhoto.Thumbnail(const Value: TInputThumbnail): TInputMessagePhoto;
begin
  Result := TInputMessagePhoto(Add('thumbnail', Value));
end;

function TInputMessagePhoto.TTL(const Value: Int32): TInputMessagePhoto;
begin
  Result := TInputMessagePhoto(Add('ttl', Value));
end;

function TInputMessagePhoto.Width(const Value: Int32): TInputMessagePhoto;
begin
  Result := TInputMessagePhoto(Add('width', Value));
end;

{ TPollTypeQuiz }

function TPollTypeQuiz.CorrectOptionId(const Value: Int32): TPollTypeQuiz;
begin
  Result := TPollTypeQuiz(Add('correct_option_id', Value));
end;

constructor TPollTypeQuiz.Create;
begin
  inherited Create('pollTypeQuiz');
end;

function TPollTypeQuiz.Explanation(const Value: TFormattedText): TPollTypeQuiz;
begin
  Result := TPollTypeQuiz(Add('explanation', Value));
end;

{ TPollTypeRegular }

function TPollTypeRegular.AllowMultipleAnswers(const Value: Boolean): TPollTypeRegular;
begin
  Result := TPollTypeRegular(Add('allow_multiple_answers', Value));
end;

constructor TPollTypeRegular.Create;
begin
  inherited Create('pollTypeRegular');
end;

{ TInputMessagePoll }

function TInputMessagePoll.CloseDate(const Value: TDateTime): TInputMessagePoll;
begin
  Result := TInputMessagePoll(Add('close_date', Value));
end;

constructor TInputMessagePoll.Create;
begin
  inherited Create('inputMessagePoll');
end;

function TInputMessagePoll.IsAnonymous(const Value: Boolean): TInputMessagePoll;
begin
  Result := TInputMessagePoll(Add('close_date', Value));
end;

function TInputMessagePoll.IsClosed(const Value: Boolean): TInputMessagePoll;
begin
  Result := TInputMessagePoll(Add('is_closed', Value));
end;

function TInputMessagePoll.OpenPeriod(const Value: Int32): TInputMessagePoll;
begin
  Result := TInputMessagePoll(Add('open_period', Value));
end;

function TInputMessagePoll.Options(const Value: TArray<string>): TInputMessagePoll;
begin
  Result := TInputMessagePoll(Add('options', Value));
end;

function TInputMessagePoll.Question(const Value: string): TInputMessagePoll;
begin
  Result := TInputMessagePoll(Add('question', Value));
end;

function TInputMessagePoll.&Type(const Value: TPollType): TInputMessagePoll;
begin
  Result := TInputMessagePoll(Add('type', Value));
end;

{ TInputMessageSticker }

constructor TInputMessageSticker.Create;
begin
  inherited Create('inputMessageSticker');
end;

function TInputMessageSticker.Emoji(const Value: string): TInputMessageSticker;
begin
  Result := TInputMessageSticker(Add('emoji', Value));
end;

function TInputMessageSticker.Height(const Value: Int32): TInputMessageSticker;
begin
  Result := TInputMessageSticker(Add('height', Value));
end;

function TInputMessageSticker.Sticker(const Value: TInputFile): TInputMessageSticker;
begin
  Result := TInputMessageSticker(Add('sticker', Value));
end;

function TInputMessageSticker.Thumbnail(const Value: TInputThumbnail): TInputMessageSticker;
begin
  Result := TInputMessageSticker(Add('thumbnail', Value));
end;

function TInputMessageSticker.Width(const Value: Int32): TInputMessageSticker;
begin
  Result := TInputMessageSticker(Add('width', Value));
end;

{ TVenue }

function TVenue.Address(const Value: string): TVenue;
begin
  Result := TVenue(Add('address', Value));
end;

constructor TVenue.Create;
begin
  inherited Create('venue');
end;

function TVenue.Id(const Value: string): TVenue;
begin
  Result := TVenue(Add('id', Value));
end;

function TVenue.Location(const Value: TLocation): TVenue;
begin
  Result := TVenue(Add('location', Value));
end;

function TVenue.Provider(const Value: string): TVenue;
begin
  Result := TVenue(Add('provider', Value));
end;

function TVenue.Title(const Value: string): TVenue;
begin
  Result := TVenue(Add('title', Value));
end;

function TVenue.&Type(const Value: string): TVenue;
begin
  Result := TVenue(Add('type', Value));
end;

{ TInputMessageVenue }

constructor TInputMessageVenue.Create;
begin
  inherited Create('inputMessageVenue');
end;

function TInputMessageVenue.Venue(const Value: TVenue): TInputMessageVenue;
begin
  Result := TInputMessageVenue(Add('venue', Value));
end;

{ TInputMessageVideo }

function TInputMessageVideo.AddedStickerFileIds(const Value: TArray<Int32>): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('added_sticker_file_ids', Value));
end;

function TInputMessageVideo.Caption(const Value: TFormattedText): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('caption', Value));
end;

constructor TInputMessageVideo.Create;
begin
  inherited Create('inputMessageVideo');
end;

function TInputMessageVideo.Duration(const Value: Int32): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('duration', Value));
end;

function TInputMessageVideo.Height(const Value: Int32): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('height', Value));
end;

function TInputMessageVideo.SupportsStreaming(const Value: Boolean): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('supports_streaming', Value));
end;

function TInputMessageVideo.Thumbnail(const Value: TInputThumbnail): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('thumbnail', Value));
end;

function TInputMessageVideo.TTL(const Value: Int32): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('ttl', Value));
end;

function TInputMessageVideo.Video(const Value: TInputFile): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('video', Value));
end;

function TInputMessageVideo.Width(const Value: Int32): TInputMessageVideo;
begin
  Result := TInputMessageVideo(Add('width', Value));
end;

{ TInputMessageVideoNote }

constructor TInputMessageVideoNote.Create;
begin
  inherited Create('inputMessageVideoNote');
end;

function TInputMessageVideoNote.Duration(const Value: Int32): TInputMessageVideoNote;
begin
  Result := TInputMessageVideoNote(Add('duration', Value));
end;

function TInputMessageVideoNote.Length(const Value: Int32): TInputMessageVideoNote;
begin
  Result := TInputMessageVideoNote(Add('length', Value));
end;

function TInputMessageVideoNote.Thumbnail(const Value: TInputThumbnail): TInputMessageVideoNote;
begin
  Result := TInputMessageVideoNote(Add('thumbnail', Value));
end;

function TInputMessageVideoNote.VideoNote(const Value: TInputFile): TInputMessageVideoNote;
begin
  Result := TInputMessageVideoNote(Add('video_note', Value));
end;

{ TInputMessageVoiceNote }

function TInputMessageVoiceNote.Caption(const Value: TFormattedText): TInputMessageVoiceNote;
begin
  Result := TInputMessageVoiceNote(Add('caption', Value));
end;

constructor TInputMessageVoiceNote.Create;
begin
  inherited Create('inputMessageVoiceNote');
end;

function TInputMessageVoiceNote.Duration(const Value: Int32): TInputMessageVoiceNote;
begin
  Result := TInputMessageVoiceNote(Add('duration', Value));
end;

function TInputMessageVoiceNote.VoiceNote(const Value: TInputFile): TInputMessageVoiceNote;
begin
  Result := TInputMessageVoiceNote(Add('voice_note', Value));
end;

function TInputMessageVoiceNote.Waveform(const Value: TBytes): TInputMessageVoiceNote;
begin
  Result := TInputMessageVoiceNote(Add('waveform', Value));
end;

{ TMessageSendOptions }

constructor TMessageSendOptions.Create;
begin
  inherited Create('messageSendOptions');
end;

function TMessageSendOptions.DisableNotification(const Value: Boolean): TMessageSendOptions;
begin
  Result := TMessageSendOptions(Add('disable_notification', Value));
end;

function TMessageSendOptions.FromBackground(const Value: Boolean): TMessageSendOptions;
begin
  Result := TMessageSendOptions(Add('from_background', Value));
end;

function TMessageSendOptions.SchedulingState(const Value: TMessageSchedulingState): TMessageSendOptions;
begin
  Result := TMessageSendOptions(Add('scheduling_state', Value));
end;

{ TMessageSchedulingStateSendAtDate }

constructor TMessageSchedulingStateSendAtDate.Create;
begin
  inherited Create('messageSchedulingStateSendAtDate');
end;

function TMessageSchedulingStateSendAtDate.SendDate(const Value: TDateTime): TMessageSchedulingStateSendAtDate;
begin
  Result := TMessageSchedulingStateSendAtDate(Add('send_date', Value));
end;

{ TMessageSchedulingStateSendWhenOnline }

constructor TMessageSchedulingStateSendWhenOnline.Create;
begin
  inherited Create('messageSchedulingStateSendWhenOnline');
end;

{ TReplyMarkupForceReply }

constructor TReplyMarkupForceReply.Create;
begin
  inherited Create('replyMarkupForceReply');
end;

function TReplyMarkupForceReply.InputFieldPlaceholder(const Value: string): TReplyMarkupForceReply;
begin
  Result := TReplyMarkupForceReply(Add('input_field_placeholder', Value));
end;

function TReplyMarkupForceReply.IsPersonal(const Value: Boolean): TReplyMarkupForceReply;
begin
  Result := TReplyMarkupForceReply(Add('is_personal', Value));
end;

{ TReplyMarkupInlineKeyboard }

constructor TReplyMarkupInlineKeyboard.Create;
begin
  inherited Create('replyMarkupInlineKeyboard');
end;

function TReplyMarkupInlineKeyboard.Rows(const Value: TArray<TArray<TInlineKeyboardButton>>): TReplyMarkupInlineKeyboard;
begin
  Result := TReplyMarkupInlineKeyboard(Add('rows', TArray<TArray<TJSONParam>>(Value)));
end;

{ TInlineKeyboardButtonTypeBuy }

constructor TInlineKeyboardButtonTypeBuy.Create;
begin
  inherited Create('inlineKeyboardButtonTypeBuy');
end;

{ TInlineKeyboardButtonTypeCallback }

constructor TInlineKeyboardButtonTypeCallback.Create;
begin
  inherited Create('inlineKeyboardButtonTypeCallback');
end;

function TInlineKeyboardButtonTypeCallback.Data(const Value: TBytes): TInlineKeyboardButtonTypeCallback;
begin
  Result := TInlineKeyboardButtonTypeCallback(Add('data', Value));
end;

{ TInlineKeyboardButtonTypeCallbackGame }

constructor TInlineKeyboardButtonTypeCallbackGame.Create;
begin
  inherited Create('inlineKeyboardButtonTypeCallbackGame');
end;

{ TInlineKeyboardButtonTypeCallbackWithPassword }

constructor TInlineKeyboardButtonTypeCallbackWithPassword.Create;
begin
  inherited Create('inlineKeyboardButtonTypeCallbackWithPassword');
end;

function TInlineKeyboardButtonTypeCallbackWithPassword.Data(const Value: TBytes): TInlineKeyboardButtonTypeCallbackWithPassword;
begin
  Result := TInlineKeyboardButtonTypeCallbackWithPassword(Add('data', Value));
end;

{ TInlineKeyboardButtonTypeLoginUrl }

constructor TInlineKeyboardButtonTypeLoginUrl.Create;
begin
  inherited Create('inlineKeyboardButtonTypeLoginUrl');
end;

function TInlineKeyboardButtonTypeLoginUrl.ForwardText(const Value: string): TInlineKeyboardButtonTypeLoginUrl;
begin
  Result := TInlineKeyboardButtonTypeLoginUrl(Add('forward_text', Value));
end;

function TInlineKeyboardButtonTypeLoginUrl.Id(const Value: Int64): TInlineKeyboardButtonTypeLoginUrl;
begin
  Result := TInlineKeyboardButtonTypeLoginUrl(Add('id', Value));
end;

function TInlineKeyboardButtonTypeLoginUrl.Url(const Value: string): TInlineKeyboardButtonTypeLoginUrl;
begin
  Result := TInlineKeyboardButtonTypeLoginUrl(Add('url', Value));
end;

{ TInlineKeyboardButtonTypeSwitchInline }

constructor TInlineKeyboardButtonTypeSwitchInline.Create;
begin
  inherited Create('inlineKeyboardButtonTypeSwitchInline');
end;

function TInlineKeyboardButtonTypeSwitchInline.InCurrentChat(const Value: Boolean): TInlineKeyboardButtonTypeSwitchInline;
begin
  Result := TInlineKeyboardButtonTypeSwitchInline(Add('in_current_chat', Value));
end;

function TInlineKeyboardButtonTypeSwitchInline.Query(const Value: string): TInlineKeyboardButtonTypeSwitchInline;
begin
  Result := TInlineKeyboardButtonTypeSwitchInline(Add('query', Value));
end;

{ TInlineKeyboardButtonTypeUrl }

constructor TInlineKeyboardButtonTypeUrl.Create;
begin
  inherited Create('inlineKeyboardButtonTypeUrl');
end;

function TInlineKeyboardButtonTypeUrl.Url(const Value: string): TInlineKeyboardButtonTypeUrl;
begin
  Result := TInlineKeyboardButtonTypeUrl(Add('url', Value));
end;

{ TInlineKeyboardButtonTypeUser }

constructor TInlineKeyboardButtonTypeUser.Create;
begin
  inherited Create('inlineKeyboardButtonTypeUser');
end;

function TInlineKeyboardButtonTypeUser.UserId(const Value: Int64): TInlineKeyboardButtonTypeUser;
begin
  Result := TInlineKeyboardButtonTypeUser(Add('user_id', Value));
end;

{ TInlineKeyboardButton }

constructor TInlineKeyboardButton.Create;
begin
  inherited Create('inlineKeyboardButton');
end;

function TInlineKeyboardButton.Text(const Value: string): TInlineKeyboardButton;
begin
  Result := TInlineKeyboardButton(Add('text', Value));
end;

function TInlineKeyboardButton.&Type(const Value: TInlineKeyboardButtonType): TInlineKeyboardButton;
begin
  Result := TInlineKeyboardButton(Add('type', Value));
end;

end.

