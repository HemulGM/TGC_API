unit TGC.Entity.BotCommand;

interface

type
  /// <summary>
  /// Represents a command supported by a bot.
  /// </summary>
  TtgBotCommand = class
  private
    FCommand: string;
    FDescription: string;
  public
    /// <summary>
    /// Text of the bot command.
    /// </summary>
    property Command: string read FCommand write FCommand;
    /// <summary>
    /// Description of the bot command.
    /// </summary>
    property Description: string read FDescription write FDescription;
  end;

implementation

end.

