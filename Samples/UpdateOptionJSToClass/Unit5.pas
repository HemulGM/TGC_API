unit Unit5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.StdCtrls,
  System.JSON, System.Generics.Collections;

type
  TProp = record
    AType: string;
    AName: string;
    FName: string;
  end;

  TForm5 = class(TForm)
    MemoIn: TMemo;
    MemoOut: TMemo;
    Layout1: TLayout;
    ButtonProc: TButton;
    procedure ButtonProcClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProps: TList<TProp>;
    procedure ProcUpdateOption(JSON: TJSONObject);
    procedure BuildClass;
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.FormCreate(Sender: TObject);
begin
  FProps := TList<TProp>.Create;
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  FProps.Free;
end;

procedure TForm5.ProcUpdateOption(JSON: TJSONObject);
begin
  var Prop: TProp;
  var AType := JSON.GetValue('value.@type', '');
  if AType = 'optionValueString' then
    AType := 'string'
  else if AType = 'optionValueInteger' then
    AType := 'Int64'
  else if AType = 'optionValueBoolean' then
    AType := 'Boolean';
  Prop.AType := AType;
  Prop.AName := JSON.GetValue('name', '');
  Prop.FName := Prop.AName;
  Prop.FName[1] := UpCase(Prop.FName[1]);
  Prop.FName := 'F' + Prop.FName;
  FProps.Add(Prop);
end;

procedure TForm5.BuildClass;
begin
  MemoOut.Lines.Clear;
  MemoOut.Lines.Add('TtgOptions = class');
  MemoOut.Lines.Add('private');
  for var Prop in FProps do
    MemoOut.Lines.Add('  ' + Prop.FName + ': ' + Prop.AType + ';');
  MemoOut.Lines.Add('public');
  for var Prop in FProps do
  begin
    var FieldName := '';
    var PrevC: Char := #0;
    for var C in Prop.AName do
    begin
      if (PrevC = #0) or (PrevC = '_') then
      begin
        FieldName := FieldName + UpCase(C);
      end
      else if C <> '_' then
        FieldName := FieldName + C;
      PrevC := C;
    end;

    MemoOut.Lines.Add('  property ' + FieldName + ': ' + Prop.AType + ' read ' + Prop.FName + ';');
  end;
  MemoOut.Lines.Add('end;');
end;

procedure TForm5.ButtonProcClick(Sender: TObject);
begin
  for var Line in MemoIn.Lines do
    if Line.StartsWith('{"@type":"updateOption","name":"') then
    try
      var JSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
      if Assigned(JSON) then
      try
        ProcUpdateOption(JSON);
      finally
        JSON.Free;
      end;
    except
      //
    end;
  BuildClass;
end;

end.

