unit TGC.Entity.Sticker;

interface

uses
  TGC.Entity.Files, REST.Json.Types;

type
  /// <summary>
  /// Describes format of the thumbnail
  /// </summary>
  TtgThumbnailFormat = class
  private
    [JSONName('@type')]
    Ftype: string;
  public
    /// <summary>
    /// thumbnailFormatGif, thumbnailFormatJpeg, thumbnailFormatMpeg4, thumbnailFormatPng, thumbnailFormatTgs, and thumbnailFormatWebp
    /// </summary>
    property AType: string read Ftype write Ftype;
  end;

  /// <summary>
  /// Represents a thumbnail.
  /// </summary>
  TtgThumbnail = class
  private
    FFile: TtgFile;
    FFormat: TtgThumbnailFormat;
    FHeight: Int32;
    FWidth: Int32;
  public
    /// <summary>
    /// The thumbnail.
    /// </summary>
    property &File: TtgFile read FFile write FFile;
    /// <summary>
    /// Thumbnail format.
    /// </summary>
    property Format: TtgThumbnailFormat read FFormat write FFormat;
    /// <summary>
    /// Thumbnail height.
    /// </summary>
    property Height: Int32 read FHeight write FHeight;
    /// <summary>
    /// Thumbnail width.
    /// </summary>
    property Width: Int32 read FWidth write FWidth;
    destructor Destroy; override;
  end;

  /// <summary>
  /// A point on a Cartesian plane.
  /// </summary>
  TtgPoint = class
  private
    FX: Extended;
    FY: Extended;
  public
    /// <summary>
    /// The point's first coordinate.
    /// </summary>
    property X: Extended read FX write FX;
    /// <summary>
    /// The point's second coordinate.
    /// </summary>
    property Y: Extended read FY write FY;
  end;

  /// <summary>
  /// Represents a vector path command.
  /// </summary>
  TtgVectorPathCommand = class
  private
    [JSONName('@type')]
    Ftype: string;
    FEnd_control_point: TtgPoint;
    FEnd_point: TtgPoint;
    FStart_control_point: TtgPoint;
  public
    /// <summary>
    /// vectorPathCommandCubicBezierCurve, and vectorPathCommandLine.
    /// </summary>
    property AType: string read Ftype write Ftype;
    /// <summary>
    /// The end control point of the curve.
    /// </summary>
    property EndControlPoint: TtgPoint read FEnd_control_point write FEnd_control_point;
    /// <summary>
    /// The end point of the curve.
    /// </summary>
    property EndPoint: TtgPoint read FEnd_point write FEnd_point;
    /// <summary>
    /// The start control point of the curve.
    /// </summary>
    property StartControlPoint: TtgPoint read FStart_control_point write FStart_control_point;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a closed vector path. The path begins at the end point of the last command.
  /// </summary>
  TtgClosedVectorPath = class
  private
    FCommands: TArray<TtgVectorPathCommand>;
  public
    /// <summary>
    /// List of vector path commands.
    /// </summary>
    property Commands: TArray<TtgVectorPathCommand> read FCommands write FCommands;
    destructor Destroy; override;
  end;

  TtgStickerFullType = class
  private
    [JSONName('@type')]
    Ftype: string;
  public
    property AType: string read Ftype write Ftype;
  end;

  TtgStickerFormat = class
  private
    [JSONName('@type')]
    Ftype: string;
  public
    property AType: string read Ftype write Ftype;
  end;

  TTgMaskPosition = class
  end;

  TtgSticker = class
  private
    FEmoji: string;
    FFormat: TtgStickerFormat;
    FFull_type: TtgStickerFullType;
    FHeight: Int32;
    FId: string;
    FOutline: TArray<TtgClosedVectorPath>;
    FSet_id: string;
    FSticker: TtgFile;
    FThumbnail: TtgThumbnail;
    FWidth: Int32;
    FIs_animated: Boolean;
    FIs_mask: Boolean;
    FMask_position: TTgMaskPosition;
  public
    /// <summary>
    /// Emoji corresponding to the sticker.
    /// </summary>
    property Emoji: string read FEmoji write FEmoji;
    property Format: TtgStickerFormat read FFormat write FFormat;
    property FullType: TtgStickerFullType read FFull_type write FFull_type;
    /// <summary>
    /// True, if the sticker is an animated sticker in TGS format.
    /// </summary>
    property IsAnimated: Boolean read FIs_animated write FIs_animated;
    /// <summary>
    /// True, if the sticker is a mask.
    /// </summary>
    property IsMask: Boolean read FIs_mask write FIs_mask;
    /// <summary>
    /// Sticker height; as defined by the sender.
    /// </summary>
    property Height: Int32 read FHeight write FHeight;
    property Id: string read FId write FId;
    /// <summary>
    /// Sticker's outline represented as a list of closed vector paths; may be empty. The coordinate system origin is in the upper-left corner.
    /// </summary>
    property Outline: TArray<TtgClosedVectorPath> read FOutline write FOutline;
    property MaskPosition: TTgMaskPosition read FMask_position write FMask_position;
    /// <summary>
    /// The identifier of the sticker set to which the sticker belongs; 0 if none.
    /// </summary>
    property SetId: string read FSet_id write FSet_id;
    /// <summary>
    /// File containing the sticker.
    /// </summary>
    property Sticker: TtgFile read FSticker write FSticker;
    /// <summary>
    /// Sticker thumbnail in WEBP or JPEG format; may be null.
    /// </summary>
    property Thumbnail: TtgThumbnail read FThumbnail write FThumbnail;
    /// <summary>
    /// Sticker width; as defined by the sender.
    /// </summary>
    property Width: Int32 read FWidth write FWidth;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Describes an animated representation of an emoji.
  /// </summary>
  TtgAnimatedEmoji = class
  private
    FFitzpatrick_type: Int32;
    FSticker: TtgSticker;
    FSticker_height: Int32;
    FSticker_width: Int32;
    FSound: TtgFile;
  public
    /// <summary>
    /// Emoji modifier fitzpatrick type; 0-6; 0 if none.
    /// </summary>
    property FitzpatrickType: Int32 read FFitzpatrick_type write FFitzpatrick_type;
    /// <summary>
    /// Animated sticker for the emoji.
    /// </summary>
    property Sticker: TtgSticker read FSticker write FSticker;
    property StickerHeight: Int32 read FSticker_height write FSticker_height;
    property StickerWidth: Int32 read FSticker_width write FSticker_width;
    /// <summary>
    /// File containing the sound to be played when the animated emoji is clicked if any; may be null. The sound is encoded with the Opus codec, and stored inside an OGG container.
    /// </summary>
    property Sound: TtgFile read FSound write FSound;
    destructor Destroy; override;
  end;

implementation

{ TtgThumbnail }

destructor TtgThumbnail.Destroy;
begin
  FFormat.free;
  FFile.free;
  inherited;
end;

{ TtgVectorPathCommand }

destructor TtgVectorPathCommand.Destroy;
begin
  FStart_control_point.free;
  FEnd_control_point.free;
  FEnd_point.free;
  inherited;
end;

{ TtgClosedVectorPath }

destructor TtgClosedVectorPath.Destroy;
var
  Item: TtgVectorPathCommand;
begin
  for Item in FCommands do
    Item.Free;
  inherited;
end;

{ TtgSticker }

destructor TtgSticker.Destroy;
var
  Item: TtgClosedVectorPath;
begin
  for Item in FOutline do
    Item.Free;
  FFormat.Free;
  FFull_type.Free;
  FThumbnail.Free;
  FSticker.Free;
  FMask_position.Free;
  inherited;
end;

{ TtgAnimatedEmoji }

destructor TtgAnimatedEmoji.Destroy;
begin
  FSticker.Free;
  inherited;
end;

end.

