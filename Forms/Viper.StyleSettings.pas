unit Viper.StyleSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  IniFiles, SvcEntry;

type
  TPaintStyle = record
    Font: TFont;
    BgColor: TColor;
  end;
  PPaintStyle = ^TPaintStyle;

  TStyleElement = (seBgColor, seFont);
  TStyleElements = set of TStyleElement;

  TStyleSettingsForm = class(TForm)
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    pnlStateStopped: TPanel;
    pnlStateStarting: TPanel;
    pnlStateStopping: TPanel;
    pnlStateRunning: TPanel;
    pnlStateResuming: TPanel;
    pnlStatePausing: TPanel;
    pnlStateOther: TPanel;
    pnlStartAuto: TPanel;
    pnlStartManual: TPanel;
    pnlStartDisabled: TPanel;
    pnlStartAutoBoot: TPanel;
    pnlTypeService: TPanel;
    pnlTypeDriver: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    pnlStartAutoDelayed: TPanel;
    pnlHasTriggers: TPanel;
    pnlTypeProtected: TPanel;
    Label3: TLabel;
    btnClose: TButton;
    btnReset: TButton;
    pnlStatePaused: TPanel;
    btnOk: TButton;
    pnlTypeInteractive: TPanel;
    pnlTypeUser: TPanel;
    procedure pnlTypeServiceClick(Sender: TObject);
    procedure pnlTypeServiceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure btnResetClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    procedure DoLoadStyles(Sender: TObject; Ini: TCustomIniFile);
    procedure DoSaveStyles(Sender: TObject; Ini: TCustomIniFile);
  public
    procedure ResetStyles;
    procedure SaveStyles;
    procedure LoadStyles;
    procedure GetCombinedStyle(Service: TServiceEntry; var Result: TPaintStyle;
      Elements: TStyleElements = [seBgColor, seFont]);
    function GetCombinedBgColor(Service: TServiceEntry): TColor;
    procedure GetCombinedFont(Service: TServiceEntry; Result: TFont);
  end;

var
  StyleSettingsForm: TStyleSettingsForm;

implementation
uses UITypes, WinSvc, ServiceHelper, Viper.StyleEdit, Viper.Settings;

{$R *.dfm}


procedure TStyleSettingsForm.FormCreate(Sender: TObject);
begin
  SettingsForm.OnLoadSettings.Add(Self.DoLoadStyles);
  SettingsForm.OnSaveSettings.Add(Self.DoSaveStyles);
end;

procedure TStyleSettingsForm.FormDestroy(Sender: TObject);
begin
  if SettingsForm <> nil then begin
    SettingsForm.OnLoadSettings.Remove(Self.DoLoadStyles);
    SettingsForm.OnSaveSettings.Remove(Self.DoSaveStyles);
  end;
end;


procedure SetColors(Panel: TPanel; BgColor: TColor; Font: TFont);
begin
  Panel.Color := BgColor;
  Panel.Font.Assign(Font);
end;

procedure TStyleSettingsForm.ResetStyles;
var DefaultFont: TFont;
begin
  DefaultFont := TFont.Create;
  try
    //Start with the font that is used by the application by default
    DefaultFont.Assign(Self.Font); //Assuming this form's Font value wasn't changed
    DefaultFont.Style := [];

    SetColors(pnlTypeService, clWindow, DefaultFont);
    SetColors(pnlTypeUser, clWindow, DefaultFont);
    pnlTypeUser.Font.Color := clGreen;
    SetColors(pnlTypeDriver, clWindow, DefaultFont);
    pnlTypeDriver.Font.Color := $00005B5B;
    SetColors(pnlTypeProtected, clWindow, DefaultFont);
    pnlTypeProtected.Font.Color := $009D009D;
    SetColors(pnlTypeInteractive, clWindow, DefaultFont);
    pnlTypeInteractive.Font.Color := clNavy;

    SetColors(pnlStartAuto, $00FFF7DD, DefaultFont);
    SetColors(pnlStartAutoBoot, $00FFF7DD, DefaultFont);
    SetColors(pnlStartAutoDelayed, $00FFF7DD, DefaultFont);
    SetColors(pnlStartManual, clWindow, DefaultFont);
    SetColors(pnlStartDisabled, clWindow, DefaultFont);
    pnlStartDisabled.Font.Color := $00AAAAAA;

    SetColors(pnlHasTriggers, $00EAFFFF, DefaultFont); //$00DDF5FF?

    SetColors(pnlStateStopped, clWindow, DefaultFont);
    DefaultFont.Style := [fsBold];
    SetColors(pnlStateStarting, clWindow, DefaultFont);
    SetColors(pnlStateStopping, clWindow, DefaultFont);
    SetColors(pnlStateRunning, clWindow, DefaultFont);
    SetColors(pnlStateResuming, clWindow, DefaultFont);
    SetColors(pnlStatePausing, clWindow, DefaultFont);
    SetColors(pnlStatePaused, clWindow, DefaultFont);
    pnlStatePaused.Font.Color := clGrayText;
    SetColors(pnlStateOther, clWindow, DefaultFont);
  finally
    FreeAndNil(DefaultFont);
  end;
end;


procedure SaveStyle(ini: TCustomIniFile; StyleControl: TPanel);
var strval: string;
begin
  strval := IntToStr(Integer(StyleControl.Color))+';'
    +IntToStr(Integer(StyleControl.Font.Color))+';'
    +IntToStr(Byte(StyleControl.Font.Style));
  ini.WriteString('Styles', StyleControl.Name, strval);
end;

function cutpart(var str: string; sep: char; out part: string): boolean;
var i: integer;
begin
  i := pos(sep, str);
  if i > 0 then begin
    part := copy(str, 1, i-1);
    delete(str, 1, i);
    Result := true;
  end else begin //last part
    Result := Length(str) > 0;
    if Result then begin
      part := str;
      str := '';
    end;
  end;
end;

procedure LoadStyle(ini: TCustomIniFile; StyleControl: TPanel);
var strval, part: string;
  val: integer;
begin
  strval := ini.ReadString('Styles', StyleControl.Name, '');
  if strval = '' then exit;

  if cutpart(strval, ';', part) and TryStrToInt(part, val) then
    StyleControl.Color := TColor(val);
  if cutpart(strval, ';', part) and TryStrToInt(part, val) then
    StyleControl.Font.Color := TColor(val);
  if cutpart(strval, ';', part) and TryStrToInt(part, val) then
    StyleControl.Font.Style := TFontStyles(byte(val));
end;

procedure TStyleSettingsForm.DoLoadStyles(Sender: TObject; Ini: TCustomIniFile);
begin
  LoadStyle(ini, pnlTypeService);
  LoadStyle(ini, pnlTypeUser);
  LoadStyle(ini, pnlTypeDriver);
  LoadStyle(ini, pnlTypeProtected);
  LoadStyle(ini, pnlTypeInteractive);
  LoadStyle(ini, pnlStartAuto);
  LoadStyle(ini, pnlStartAutoBoot);
  LoadStyle(ini, pnlStartManual);
  LoadStyle(ini, pnlStartDisabled);
  LoadStyle(ini, pnlHasTriggers);
  LoadStyle(ini, pnlStateStopped);
  LoadStyle(ini, pnlStateStarting);
  LoadStyle(ini, pnlStateStopping);
  LoadStyle(ini, pnlStateRunning);
  LoadStyle(ini, pnlStateResuming);
  LoadStyle(ini, pnlStatePausing);
  LoadStyle(ini, pnlStatePaused);
  LoadStyle(ini, pnlStateOther);
end;

procedure TStyleSettingsForm.DoSaveStyles(Sender: TObject; Ini: TCustomIniFile);
begin
  SaveStyle(ini, pnlTypeService);
  SaveStyle(ini, pnlTypeUser);
  SaveStyle(ini, pnlTypeDriver);
  SaveStyle(ini, pnlTypeProtected);
  SaveStyle(ini, pnlTypeInteractive);
  SaveStyle(ini, pnlStartAuto);
  SaveStyle(ini, pnlStartAutoBoot);
  SaveStyle(ini, pnlStartManual);
  SaveStyle(ini, pnlStartDisabled);
  SaveStyle(ini, pnlHasTriggers);
  SaveStyle(ini, pnlStateStopped);
  SaveStyle(ini, pnlStateStarting);
  SaveStyle(ini, pnlStateStopping);
  SaveStyle(ini, pnlStateRunning);
  SaveStyle(ini, pnlStateResuming);
  SaveStyle(ini, pnlStatePausing);
  SaveStyle(ini, pnlStatePaused);
  SaveStyle(ini, pnlStateOther);
end;

procedure TStyleSettingsForm.LoadStyles;
var ini: TCustomIniFile;
begin
  ResetStyles;
  ini := Viper.Settings.GetSettings;
  try
    DoLoadStyles(Self, ini);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TStyleSettingsForm.SaveStyles;
var ini: TCustomIniFile;
begin
  ini := Viper.Settings.GetSettings;
  try
    DoSaveStyles(Self, ini);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TStyleSettingsForm.btnResetClick(Sender: TObject);
begin
  ResetStyles;
end;

procedure TStyleSettingsForm.btnOkClick(Sender: TObject);
begin
  SaveStyles;
  ModalResult := mrOk;
end;

procedure TStyleSettingsForm.btnCloseClick(Sender: TObject);
begin
  LoadStyles;
  ModalResult := mrCancel;
end;


procedure TStyleSettingsForm.pnlTypeServiceClick(Sender: TObject);
var StyleEditForm: TStyleEditForm;
begin
  StyleEditForm := TStyleEditForm.Create(Self);
  try
    StyleEditForm.cbBgColor.Selected := TPanel(Sender).Color;
    StyleEditForm.cbFontColor.Selected := TPanel(Sender).Font.Color;
    StyleEditForm.FontStyles := TPanel(Sender).Font.Style;
    if IsPositiveResult(StyleEditForm.ShowModal) then begin
      TPanel(Sender).Color := StyleEditForm.cbBgColor.Selected;
      TPanel(Sender).Font.Color := StyleEditForm.cbFontColor.Selected;
      TPanel(Sender).Font.Style := StyleEditForm.FontStyles;
    end;
  finally
    FreeAndNil(StyleEditForm);
  end;
{
Alternatively:
  FontDialog.Font.Assign(TPanel(Sender).Font);
  if FontDialog.Execute(Self.Handle) then
    TPanel(Sender).Font.Assign(FontDialog.Font);
}
end;

procedure TStyleSettingsForm.pnlTypeServiceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then begin
    ColorDialog.Color := TPanel(Sender).Color;
    if ColorDialog.Execute(Self.Handle) then
      TPanel(Sender).Color := ColorDialog.Color;
  end;
end;


//True if the Adjustment in question has any non-standard settings
function HasStyleChanges(Adjustment: TPanel; Elements: TStyleElements): boolean;
begin
  Result := ((seBgColor in Elements) and (Adjustment.Color <> clWindow))
    or ((seFont in Elements) and ((Adjustment.Font.Style <> []) or (Adjustment.Font.Color <> clWindowText)))
end;

//Apply any non-standard settings from Adjustment font to the target Style
procedure AdjustStyle(var Style: TPaintStyle; Adjustment: TPanel; Elements: TStyleElements); inline;
begin
  if seBgColor in Elements then
    if Adjustment.Color <> clWindow then Style.BgColor := Adjustment.Color;
  if seFont in Elements then begin
    if Adjustment.Font.Style <> [] then Style.Font.Style := Style.Font.Style + Adjustment.Font.Style;
    if Adjustment.Font.Color <> clWindowText then Style.Font.Color := Adjustment.Font.Color;
  end;
end;

//Calculates the combined color and font style for a given service
//If no override is configured for some aspect of the style, the original value will be kept
procedure TStyleSettingsForm.GetCombinedStyle(Service: TServiceEntry; var Result: TPaintStyle;
  Elements: TStyleElements);
begin
  if Service.Status.dwServiceType and SERVICE_DRIVER <> 0 then
    AdjustStyle(Result, pnlTypeDriver, Elements)
  else
  if Service.Status.dwServiceType and SERVICE_USER_SERVICE <> 0 then
    AdjustStyle(Result, pnlTypeUser, Elements)
  else
  if Service.Status.dwServiceType and SERVICE_USERSERVICE_INSTANCE <> 0 then
    AdjustStyle(Result, pnlTypeUser, Elements)
  else
  if Service.Status.dwServiceType and SERVICE_INTERACTIVE_PROCESS <> 0 then
    AdjustStyle(Result, pnlTypeInteractive, Elements)
  else
    AdjustStyle(Result, pnlTypeService, Elements);

  //First check for changes because checking for IsLaunchProtected might be expensive
  if HasStyleChanges(pnlTypeProtected, Elements) then
    if Service.IsLaunchProtected then AdjustStyle(Result, pnlTypeProtected, Elements);

  if Service.Config <> nil then
    case Service.Config.dwStartType of
      SERVICE_BOOT_START,
      SERVICE_SYSTEM_START:
        AdjustStyle(Result, pnlStartAutoBoot, Elements);
      SERVICE_AUTO_START:
        AdjustStyle(Result, pnlStartAuto, Elements);
      SERVICE_DEMAND_START:
        AdjustStyle(Result, pnlStartManual, Elements);
     //We will process DISABLED after we process triggers because DISABLED overrides triggers
    end;

  if HasStyleChanges(pnlHasTriggers, Elements) then
    if Service.TriggerCount > 0 then AdjustStyle(Result, pnlHasTriggers, Elements);

  if Service.Config <> nil then
   //DISABLED overrides triggers
    if Service.Config.dwStartType = SERVICE_DISABLED then
      AdjustStyle(Result, pnlStartDisabled, Elements);

  case Service.Status.dwCurrentState of
    SERVICE_STOPPED:
      AdjustStyle(Result, pnlStateStopped, Elements);
     SERVICE_START_PENDING:
      AdjustStyle(Result, pnlStateStarting, Elements);
    SERVICE_STOP_PENDING:
      AdjustStyle(Result, pnlStateStopping, Elements);
    SERVICE_RUNNING:
      AdjustStyle(Result, pnlStateRunning, Elements);
    SERVICE_CONTINUE_PENDING:
      AdjustStyle(Result, pnlStateResuming, Elements);
    SERVICE_PAUSE_PENDING:
      AdjustStyle(Result, pnlStatePausing, Elements);
    SERVICE_PAUSED:
      AdjustStyle(Result, pnlStatePaused, Elements);
  else
    AdjustStyle(Result, pnlStateStopped, Elements);
  end;
end;

function TStyleSettingsForm.GetCombinedBgColor(Service: TServiceEntry): TColor;
var Style: TPaintStyle;
begin
  Style.BgColor := clWindow;
  GetCombinedStyle(Service, Style, [seBgColor]);
  Result := Style.BgColor;
end;

procedure TStyleSettingsForm.GetCombinedFont(Service: TServiceEntry; Result: TFont);
var Style: TPaintStyle;
begin
  Style.Font := Result; //font is an object
  GetCombinedStyle(Service, Style, [seFont]);
end;



end.
