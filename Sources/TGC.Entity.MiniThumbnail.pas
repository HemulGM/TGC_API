unit TGC.Entity.MiniThumbnail;

interface

uses
  System.SysUtils;

type
  /// <summary>
  /// Thumbnail image of a very poor quality and low resolution.
  /// </summary>
  TtgMinithumbnail = class
  private
    FData: string;
    FHeight: Int64;
    FWidth: Int64;
  public
    /// <summary>
    /// The thumbnail in JPEG format. (base64)
    /// </summary>
    property Data: string read FData write FData;
    /// <summary>
    /// Thumbnail height, usually doesn't exceed 40.
    /// </summary>
    property Height: Int64 read FHeight write FHeight;
    /// <summary>
    /// Thumbnail width, usually doesn't exceed 40.
    /// </summary>
    property Width: Int64 read FWidth write FWidth;
  end;

implementation

end.

