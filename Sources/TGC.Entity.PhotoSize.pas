unit TGC.Entity.PhotoSize;

interface

uses
  TGC.Entity.Files;

type
  /// <summary>
  /// Describes an image in JPEG format.
  /// </summary>
  TtgPhotoSize = class
  private
    FType: string;
    FPhoto: TtgFile;
    FWidth: Int32;
    FHeight: Int32;
    FProgressive_sizes: TArray<Int32>;
  public
    /// <summary>
    /// Image type (see https://core.telegram.org/constructor/photoSize).
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Information about the image file.
    /// </summary>
    property Photo: TtgFile read FPhoto write FPhoto;
    /// <summary>
    /// Image width.
    /// </summary>
    property Width: Int32 read FWidth write FWidth;
    /// <summary>
    /// Image height.
    /// </summary>
    property Height: Int32 read FHeight write FHeight;
    /// <summary>
    /// Sizes of progressive JPEG file prefixes, which can be used to preliminarily show the image; in bytes.
    /// </summary>
    property ProgressiveSizes: TArray<Int32> read FProgressive_sizes write FProgressive_sizes;
    destructor Destroy; override;
  end;

implementation

{ TtgPhotoSize }

destructor TtgPhotoSize.Destroy;
begin
  FPhoto.Free;
  inherited;
end;

end.

