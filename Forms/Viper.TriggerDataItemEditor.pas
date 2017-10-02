unit Viper.TriggerDataItemEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WinSvc;

type
  TTriggerDataItemEditor = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lblValue: TLabel;
    lblDataType: TLabel;
    cbType: TComboBox;
    edtValue: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  protected
    FData: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
    procedure ReloadTypes;
    procedure LoadData;
    procedure SaveData;
  public
    function GetData: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
    procedure CopyData(const AData: PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM);

  end;

var
  TriggerDataItemEditor: TTriggerDataItemEditor;

implementation
uses UniStrUtils, ServiceHelper, TriggerUtils;

{$R *.dfm}

procedure TTriggerDataItemEditor.FormCreate(Sender: TObject);
begin
  ReloadTypes;
  FData.dwDataType := SERVICE_TRIGGER_DATA_TYPE_STRING;
  FData.cbData := 0;
  FData.pData := nil;
  LoadData; //load the default state in case we're called for new item
end;

procedure TTriggerDataItemEditor.FormDestroy(Sender: TObject);
begin
  if FData.pData <> nil then
    Self.CopyData(nil);
end;

function TTriggerDataItemEditor.GetData: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
begin
  Result := FData;
end;

procedure TTriggerDataItemEditor.CopyData(const AData: PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM);
begin
  if FData.pData <> nil then
    FreeStandaloneTriggerDataItem(FData);

  if AData <> nil then
    FData := CopyTriggerDataItem(AData^);

  LoadData;
end;

procedure TTriggerDataItemEditor.ReloadTypes;
begin
  cbType.Clear;
  cbType.AddItem(sTriggerDataTypeBinary, TObject(SERVICE_TRIGGER_DATA_TYPE_BINARY));
  cbType.AddItem(sTriggerDataTypeString, TObject(SERVICE_TRIGGER_DATA_TYPE_STRING));
  cbType.AddItem(sTriggerDataTypeLevel, TObject(SERVICE_TRIGGER_DATA_TYPE_LEVEL));
  cbType.AddItem(sTriggerDataTypeKeywordAny, TObject(SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ANY));
  cbType.AddItem(sTriggerDataTypeKeywordAll, TObject(SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ALL));
end;

//Loads any values in the form from the associated FData structure.
procedure TTriggerDataItemEditor.LoadData;
var i, idx: integer;
begin
  idx := -1;
  for i := 0 to cbType.Items.Count-1 do
    if cardinal(cbType.Items.Objects[i]) = FData.dwDataType then begin
      idx := i;
      break;
    end;
  if idx >= 0 then begin
    cbType.Style := csDropDownList;
    cbType.ItemIndex := idx;
  end else begin
    cbType.Style := csDropDown;
    cbType.Text := IntToStr(idx);
  end;

  case FData.dwDataType of
    SERVICE_TRIGGER_DATA_TYPE_STRING: edtValue.Text := FData.StringValue;
    SERVICE_TRIGGER_DATA_TYPE_LEVEL: edtValue.Text := IntToStr(FData.ByteValue);
    SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ANY: edtValue.Text := IntToHex(FData.Int64Value, 2);
    SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ALL: edtValue.Text := IntToHex(FData.Int64Value, 2);
  else
   //SERVICE_TRIGGER_DATA_TYPE_BINARY as well as any unknown types
    edtValue.Text := FData.HexValue;
  end;
end;

resourcestring
  sPleaseSelectDataType = 'Please select a data type';

//Verifies any values entered in the form and saves them into an associated FData structure.
procedure TTriggerDataItemEditor.SaveData;
var tmpInt: int64;
begin
 //cbType can be in drop-down mode (the default) or free editing mode (if the data type was unusual)
  if cbType.ItemIndex >= 0 then
    FData.dwDataType := cardinal(cbType.Items.Objects[cbType.ItemIndex])
  else
    if cbType.Style = csDropDownList then
      raise Exception.Create(sPleaseSelectDataType)
    else
      FData.dwDataType := StrToInt(cbType.Text); //free edited type must be int

 //Convert textual value into the appropriate data format
 //FData is allocated by us so can resize.
  case FData.dwDataType of
    SERVICE_TRIGGER_DATA_TYPE_STRING: FData.SetStringValue(edtValue.Text);
    SERVICE_TRIGGER_DATA_TYPE_LEVEL: begin
      tmpInt := StrToInt(edtValue.Text);
      if (tmpInt < 0) or (tmpInt > 255) then
        raise Exception.Create('Illegal value for a byte');
      FData.SetByteValue(tmpInt);
    end;
    //The following two are edited as hex
    SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ANY: FData.SetInt64Value(StrToInt64('$'+edtValue.Text));
    SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ALL: FData.SetInt64Value(StrToInt64('$'+edtValue.Text));
  else //Binary / unknown
    FData.SetHexValue(edtValue.Text);
  end;
end;

procedure TTriggerDataItemEditor.btnOkClick(Sender: TObject);
begin
  SaveData;
  ModalResult := mrOk;
end;

end.
