unit TGC.Entity.ProfilePhoto;

interface

uses
  TGC.Entity.Files, TGC.Entity.AObject, TGC.Entity.MiniThumbnail;

type
  /// <summary>
  /// Describes a user profile photo.
  /// </summary>
  TTgProfilePhoto = class(TtgObject)
  private
    FBig: TtgFile;
    FHas_animation: Boolean;
    FId: string;
    FIs_personal: Boolean;
    FMinithumbnail: TTgMinithumbnail;
    FSmall: TtgFile;
  public
    /// <summary>
    /// A big (640x640) user profile photo. The file can be downloaded only before the photo is changed.
    /// </summary>
    property Big: TtgFile read FBig write FBig;
    /// <summary>
    /// True, if the photo has animated variant.
    /// </summary>
    property HasAnimation: Boolean read FHas_animation write FHas_animation;
    /// <summary>
    /// Photo identifier; 0 for an empty photo. Can be used to find a photo in a list of user profile photos.
    /// </summary>
    property Id: string read FId write FId;
    property IsPersonal: Boolean read FIs_personal write FIs_personal;
    /// <summary>
    /// User profile photo minithumbnail; may be null.
    /// </summary>
    property Minithumbnail: TTgMinithumbnail read FMinithumbnail write FMinithumbnail;
    /// <summary>
    /// A small (160x160) user profile photo. The file can be downloaded only before the photo is changed.
    /// </summary>
    property Small: TtgFile read FSmall write FSmall;
    destructor Destroy; override;
  end;

implementation

{ TTgProfilePhoto }

destructor TTgProfilePhoto.Destroy;
begin
  if Assigned(FMinithumbnail) then
    FMinithumbnail.Free;
  if Assigned(FSmall) then
    FSmall.Free;
  if Assigned(FBig) then
    FBig.Free;
  inherited;
end;

end.

