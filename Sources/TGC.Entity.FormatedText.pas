unit TGC.Entity.FormatedText;

interface

uses
  Rest.Json.Types;

type
  TtgTextEntityType = class
  private
    [JSONName('@type')]
    FType: string;
    FLanguage: string;
    FMedia_timestamp: Integer;
    FUser_id: Int64;
    FUrl: string;
  public
    property AType: string read FType write FType;
    /// <summary>
    /// Programming language of the code; as defined by the sender.
    /// </summary>
    property Language: string read FLanguage write FLanguage;
    /// <summary>
    /// Timestamp from which a video/audio/video note/voice note playing must start, in seconds. The media can be in the content or the web page preview of the current message, or in the same places in the replied message.
    /// </summary>
    property MediaTimestamp: Integer read FMedia_timestamp write FMedia_timestamp;
    /// <summary>
    /// Identifier of the mentioned user.
    /// </summary>
    property UserId: Int64 read FUser_id write FUser_id;
    /// <summary>
    /// HTTP or tg:// URL to be opened when the link is clicked.
    /// </summary>
    property Url: string read FUrl write FUrl;
  end;

  TtgTextEntity = class
  private
    [JSONName('@type')]
    FType: string;
    FLength: Integer;
    FOffset: Integer;
    [JSONName('type')]
    FEntityType: TtgTextEntityType;
  public
    property AType: string read Ftype write Ftype;
    property Length: Integer read FLength write FLength;
    property Offset: Integer read FOffset write FOffset;
    property EntityType: TtgTextEntityType read FEntityType write FEntityType;
    destructor Destroy; override;
  end;

  TtgFormatedText = class
  private
    FEntities: TArray<TtgTextEntity>;
    FText: string;
  public
    property Entities: TArray<TtgTextEntity> read FEntities write FEntities;
    property Text: string read FText write FText;
    destructor Destroy; override;
  end;

implementation

{ TtgFormatedText }

destructor TtgFormatedText.Destroy;
begin
  for var Item in FEntities do
    Item.Free;
  inherited;
end;

{ TtgTextEntity }

destructor TtgTextEntity.Destroy;
begin
  FEntityType.Free;
  inherited;
end;

end.

