unit TGC.Handler.UpdateOption;

interface

uses
  System.Classes, System.SysUtils, System.JSON, TGC.Handler, TGC.Options;

type
  TUpdateOption = class(THandler)
  private
    FOptions: TtgOptions;
    procedure UpdateOption(const FieldName, AType: string; Value: TJSONValue);
  public
    procedure Execute(JSON: TJSONObject); override;
    constructor Create(AClient: TObject); override;
  end;

implementation

uses
  TGC.Client, System.Rtti, System.Types;

{ TUpdateOption }

constructor TUpdateOption.Create(AClient: TObject);
begin
  inherited;
  FOptions := TTelegramClientCustom(Client).Options;
end;

procedure TUpdateOption.UpdateOption(const FieldName, AType: string; Value: TJSONValue);
var
  T: TRttiType;
  F: TRttiField;
  SharedContext: TRttiContext;
begin
  if FOptions <> nil then
  begin
    T := SharedContext.GetType(FOptions.ClassInfo);
    if T <> nil then
    begin
      F := T.GetField(FieldName);
      if (F <> nil) then
      begin
        var Val: TValue;
        if AType = 'optionValueString' then
          F.SetValue(FOptions, Value.AsType<string>)
        else if AType = 'optionValueInteger' then
          F.SetValue(FOptions, Value.AsType<Int64>)
        else if AType = 'optionValueBoolean' then
          F.SetValue(FOptions, Value.AsType<Boolean>)
      end;
    end;
  end;
end;

procedure TUpdateOption.Execute(JSON: TJSONObject);
begin
  inherited;
  var FieldName := JSON.GetValue('name', '');
  FieldName[1] := UpCase(FieldName[1]);
  FieldName := 'F' + FieldName;
  UpdateOption(FieldName, JSON.GetValue('value.@type', ''), JSON.GetValue<TJSONValue>('value.value'));
end;

end.

