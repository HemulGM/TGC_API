unit TGC.Entity.AnimatedChatPhoto;

interface

uses
  TGC.Entity.Files;

type
  /// <summary>
  /// Animated variant of a chat photo in MPEG4 format.
  /// </summary>
  TtgAnimatedChatPhoto = class
  private
    FLength: Int32;
    FFile: TtgFile;
    FMain_frame_timestamp: Double;
  public
    /// <summary>
    /// Animation width and height.
    /// </summary>
    property Length: Int32 read FLength write FLength;
    /// <summary>
    /// Information about the animation file.
    /// </summary>
    property &File: TtgFile read FFile write FFile;
    /// <summary>
    /// Timestamp of the frame, used as a static chat photo.
    /// </summary>
    property MainFrameTimestamp_: Double read FMain_frame_timestamp write FMain_frame_timestamp;
    destructor Destroy; override;
  end;

implementation

{ TtgAnimatedChatPhoto }

destructor TtgAnimatedChatPhoto.Destroy;
begin
  FFile.Free;
  inherited;
end;

end.

