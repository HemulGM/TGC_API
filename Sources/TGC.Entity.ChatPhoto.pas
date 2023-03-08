unit TGC.Entity.ChatPhoto;

interface

uses
  TGC.Entity.AObject, REST.JsonReflect, REST.Json.Interceptors,
  TGC.Entity.MiniThumbnail, TGC.Entity.PhotoSize, TGC.Entity.AnimatedChatPhoto;

type
  /// <summary>
  /// Describes a chat or user profile photo.
  /// </summary>
  TtgChatPhoto = class(TtgObject)
  private
    FId: Int64;
    [JsonReflect(ctString, rtString, TUnixDateTimeInterceptor)]
    FAdded_date: TDateTime;
    FMinithumbnail: TtgMiniThumbnail;
    FSizes: TArray<TtgPhotoSize>;
    FAnimation: TtgAnimatedChatPhoto;
  public
    /// <summary>
    /// Unique photo identifier.
    /// </summary>
    property Id: Int64 read FId write FId;
    /// <summary>
    /// Point in time (Unix timestamp) when the photo has been added.
    /// </summary>
    property AddedDate: TDateTime read FAdded_date write FAdded_date;
    /// <summary>
    /// Photo minithumbnail; may be null.
    /// </summary>
    property MiniThumbnail: TtgMiniThumbnail read FMinithumbnail write FMinithumbnail;
    /// <summary>
    /// Available variants of the photo in JPEG format, in different size.
    /// </summary>
    property Sizes: TArray<TtgPhotoSize> read FSizes write FSizes;
    /// <summary>
    /// Animated variant of the photo in MPEG4 format; may be null.
    /// </summary>
    property Animation: TtgAnimatedChatPhoto read FAnimation write FAnimation;
    destructor Destroy; override;
  end;

implementation

{ TtgChatPhoto }

destructor TtgChatPhoto.Destroy;
begin
  FMinithumbnail.Free;
  FAnimation.Free;
  for var Item in FSizes do
    Item.Free;
  inherited;
end;

end.

