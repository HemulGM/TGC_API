unit TGC.Builder.SendMessageAlbum;

interface

uses
  TGC.Classes, TGC.Builder.SendMessage;

type
  /// <summary>
  /// Sends 2-10 messages grouped together into an album. Currently, only audio, document, photo and video messages can be grouped into an album. Documents and audio files can be only grouped in an album with messages of the same type. Returns sent messages.
  /// </summary>
  TSendMessageAlbum = class(TParam)
    /// <summary>
    /// Target chat.
    /// </summary>
    function ChatId(const Value: Int64): TSendMessageAlbum;
    /// <summary>
    /// If not 0, a message thread identifier in which the message will be sent.
    /// </summary>
    function MessageThreadId(const Value: Int64): TSendMessageAlbum;
    /// <summary>
    /// Identifier of the message to reply to or 0.
    /// </summary>
    function ReplyToMessageId(const Value: Int64): TSendMessageAlbum;
    /// <summary>
    /// The content of the message to be sent.
    /// </summary>
    function InputMessageContents(const Value: TArray<TInputMessageContent>): TSendMessageAlbum;
    /// <summary>
    /// Options to be used to send the message; pass null to use default options.
    /// </summary>
    function Options(const Value: TMessageSendOptions): TSendMessageAlbum;
    constructor Create; reintroduce;
  end;

implementation

uses
  HGM.JSONParams;

{ TSendMessageAlbum }

function TSendMessageAlbum.ChatId(const Value: Int64): TSendMessageAlbum;
begin
  Result := TSendMessageAlbum(Add('chat_id', Value));
end;

constructor TSendMessageAlbum.Create;
begin
  inherited Create('sendMessageAlbum');
end;

function TSendMessageAlbum.InputMessageContents(const Value: TArray<TInputMessageContent>): TSendMessageAlbum;
begin
  Result := TSendMessageAlbum(Add('input_message_contents', TArray<TJSONParam>(Value)));
end;

function TSendMessageAlbum.MessageThreadId(const Value: Int64): TSendMessageAlbum;
begin
  Result := TSendMessageAlbum(Add('message_thread_id', Value));
end;

function TSendMessageAlbum.Options(const Value: TMessageSendOptions): TSendMessageAlbum;
begin
  Result := TSendMessageAlbum(Add('options', Value));
end;

function TSendMessageAlbum.ReplyToMessageId(const Value: Int64): TSendMessageAlbum;
begin
  Result := TSendMessageAlbum(Add('reply_to_message_id', Value));
end;

end.

