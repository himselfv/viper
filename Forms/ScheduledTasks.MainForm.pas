unit ScheduledTasks.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, Actions, ActnList, taskschd, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TNdTaskData = record
    Name: string;
    Path: string;
    StateStr: string;
    FlagsStr: string;
    URI: string;
    Source: string;
    Task: IRegisteredTask;
  end;
  PNdTaskData = ^TNdTaskData;

  TScheduledTasksMainForm = class(TForm)
    vtTasks: TVirtualStringTree;
    alActions: TActionList;
    aReload: TAction;
    pnlBottom: TPanel;
    mmDetails: TMemo;
    Splitter1: TSplitter;
    procedure vtTasksGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtTasksInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtTasksFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtTasksGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure aReloadExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtTasksFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure FormShow(Sender: TObject);
    procedure vtTasksHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtTasksCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  protected
    FTaskScheduler: ITaskService;
    function GetNodeData(Node: PVirtualNode): PNdTaskData;
  public
    procedure Clear;
    procedure Reload;
    procedure AddFolder(APath: string); overload;
    procedure AddFolder(AFolder: ITaskFolder); overload;
    function AddTask(ATask: IRegisteredTask): PVirtualNode;
  protected
    procedure ReloadFocusedProps;
    procedure ClearProps;
    procedure AddProp(const AName: string; const AValue: string); overload;
    procedure AddProp(const AName: string; const AValue: integer); overload;
    procedure AddPropHex(const AName: string; const AValue: integer); overload;
    procedure AddProp(const AName: string; const AValue: TDatetime); overload;
    procedure AddProp(const AName: string; const AValue: boolean); overload;
  end;

var
  ScheduledTasksMainForm: TScheduledTasksMainForm;

implementation
uses ActiveX, ComObj;

{$R *.dfm}

{
Info:
  https://docs.microsoft.com/en-us/windows/win32/taskschd/task-scheduler-interfaces
  https://stackoverflow.com/questions/20454115/where-does-windows-store-the-settings-for-scheduled-tasks-console
  %systemroot%\System32\Tasks
  %systemroot%\Tasks
  HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\Taskcache\Tasks
  HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\Taskcache\Tree
}


function TaskStateToStr(const AState: TASK_STATE): string;
begin
  case AState of
    TASK_STATE_UNKNOWN: Result := 'Unknown';
    TASK_STATE_DISABLED: Result := 'Disabled';
    TASK_STATE_QUEUED: Result := 'Queued';
    TASK_STATE_READY: Result := 'Ready';
    TASK_STATE_RUNNING: Result := 'Running';
  else Result := 'TaskState ('+IntToStr(integer(AState))+')';
  end;
end;

function TaskLogonTypeToStr(const AType: TASK_LOGON_TYPE): string;
begin
  case AType of
    TASK_LOGON_NONE: Result := 'None';
    TASK_LOGON_PASSWORD: Result := 'Password';
    TASK_LOGON_S4U: Result := 'S4U';
    TASK_LOGON_INTERACTIVE_TOKEN: Result := 'Interactive Token';
    TASK_LOGON_GROUP: Result := 'Group';
    TASK_LOGON_SERVICE_ACCOUNT: Result := 'Service Account';
    TASK_LOGON_INTERACTIVE_TOKEN_OR_PASSWORD: Result := 'Interactive Token/Password';
  else Result := 'LogonType ('+IntToStr(integer(AType))+')';
  end;
end;

function TaskRunLevelToStr(const ALevel: TASK_RUNLEVEL_TYPE): string;
begin
  case ALevel of
    TASK_RUNLEVEL_LUA: Result := 'LUA';
    TASK_RUNLEVEL_HIGHEST: Result := 'Highest';
  else Result := 'RunLevel ('+IntToStr(integer(ALevel))+')';
  end;
end;


procedure TScheduledTasksMainForm.FormCreate(Sender: TObject);
begin
  CoInitialize(nil);
  //Sic: CLSID_TaskScheduler but IID_ITaskService.
  //Example: https://docs.microsoft.com/en-us/windows/win32/taskschd/displaying-task-names-and-state--c---
  OleCheck(CoCreateInstance(CLSID_TaskScheduler, nil, CLSCTX_INPROC_SERVER, IID_ITaskService, Self.FTaskScheduler));
  OleCheck(Self.FTaskScheduler.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam));
end;

procedure TScheduledTasksMainForm.FormDestroy(Sender: TObject);
begin
  Clear;
  Self.FTaskScheduler := nil;
  CoUninitialize();
end;

procedure TScheduledTasksMainForm.FormShow(Sender: TObject);
begin
  Reload;
end;

procedure TScheduledTasksMainForm.vtTasksGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdTaskData);
end;

procedure TScheduledTasksMainForm.vtTasksInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var NodeData: PNdTaskData;
begin
  NodeData := Sender.GetNodeData(Node);
  Initialize(NodeData^);
end;

procedure TScheduledTasksMainForm.vtTasksFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var NodeData: PNdTaskData;
begin
  NodeData := Sender.GetNodeData(Node);
  Finalize(NodeData^);
end;

function TScheduledTasksMainForm.GetNodeData(Node: PVirtualNode): PNdTaskData;
begin
  Result := PNdTaskData(vtTasks.GetNodeData(Node));
end;

procedure TScheduledTasksMainForm.vtTasksGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var NodeData: PNdTaskData;
begin
  NodeData := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0: CellText := NodeData.Name;
    1: CellText := NodeData.Path;
    2: CellText := NodeData.StateStr;
    3: CellText := NodeData.FlagsStr;
    4: CellText := NodeData.URI;
    5: CellText := NodeData.Source;
  end;
end;

procedure TScheduledTasksMainForm.vtTasksCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  //По умолчанию, если нет специальной обработки для колонки:
  Result := CompareText(vtTasks.Text[Node1, Column], vtTasks.Text[Node2, Column]);
end;

procedure TScheduledTasksMainForm.vtTasksHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then begin
    Sender.SortColumn := HitInfo.Column;
    Sender.SortDirection := sdAscending;
  end else
    if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
    else
    if Sender.SortDirection = sdDescending then
      Sender.SortColumn := NoColumn;
  if Sender.SortColumn <> NoColumn then
    Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection);
end;

procedure TScheduledTasksMainForm.aReloadExecute(Sender: TObject);
begin
  Self.Reload;
end;

procedure TScheduledTasksMainForm.Clear;
begin
  vtTasks.Clear;
end;

procedure TScheduledTasksMainForm.Reload;
begin
  Self.Clear;
  vtTasks.BeginUpdate;
  try
    Self.AddFolder('\');
  finally
    vtTasks.EndUpdate;
  end;
end;

//The path should not end with \, unless its a single \
procedure TScheduledTasksMainForm.AddFolder(APath: string);
var AFolder: ITaskFolder;
begin
  if APath='' then
    APath := '\';
  OleCheck(Self.FTaskScheduler.GetFolder(PChar(APath), AFolder));

  Self.AddFolder(AFolder);
end;

procedure TScheduledTasksMainForm.AddFolder(AFolder: ITaskFolder);
var AFolders: ITaskFolderCollection;
  ATasks: IRegisteredTaskCollection;
  i: integer;
begin
  OleCheck(AFolder.GetTasks(integer(TASK_ENUM_HIDDEN), ATasks));
  for i := 1 to ATasks.Count do //These collections are 1-based
    AddTask(ATasks.Item[i]);

  OleCheck(AFolder.GetFolders(0, AFolders));
  for i := 1 to AFolders.Count do //These collections are 1-based
    AddFolder(AFolders.Item[i]);
end;

function TScheduledTasksMainForm.AddTask(ATask: IRegisteredTask): PVirtualNode;
var NodeData: PNdTaskData;
begin
  Result := vtTasks.AddChild(nil);
  vtTasks.ReinitNode(Result, false);
  NodeData := Self.GetNodeData(Result);

  NodeData.Name := ATask.Name;
  NodeData.Path := ATask.Path;
  NodeData.Task := ATask;
  NodeData.StateStr := TaskStateToStr(ATask.State);

  NodeData.FlagsStr := '';
  if ATask.Definition.Settings.Hidden then
    NodeData.FlagsStr :=  NodeData.FlagsStr + 'Hidden';

  NodeData.URI := ATask.Definition.RegistrationInfo.URI;
  NodeData.Source := ATask.Definition.RegistrationInfo.Source;
end;



procedure TScheduledTasksMainForm.vtTasksFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  ReloadFocusedProps;
end;

procedure TScheduledTasksMainForm.ReloadFocusedProps;
var NodeData: PNdTaskData;
  Task: IRegisteredTask;
  TaskDef: ITaskDefinition;
begin
  ClearProps;
  if vtTasks.FocusedNode = nil then
    exit;
  NodeData := Self.GetNodeData(vtTasks.FocusedNode);
  Task := NodeData.Task;

  AddProp('LastRunTime', Task.LastRunTime);
  AddPropHex('LastTaskResult', Task.LastTaskResult);
  AddProp('NextRunTime', Task.NextRunTime);
  AddProp('XmlText', Task.XML);

  TaskDef := Task.Definition;
  if SameText(Task.XML, TaskDef.XmlText) then
    AddProp('Def.XmlText', 'Same as XmlText')
  else
    AddProp('Def.XmlText', TaskDef.XmlText);

  AddProp('Description', TaskDef.RegistrationInfo.Description);
  AddProp('Author', TaskDef.RegistrationInfo.Author);
  AddProp('Version', TaskDef.RegistrationInfo.Version);
  AddProp('Date', TaskDef.RegistrationInfo.Date);
  AddProp('Documentation', TaskDef.RegistrationInfo.Documentation);
  AddProp('URI', TaskDef.RegistrationInfo.URI);
  AddProp('Source', TaskDef.RegistrationInfo.Source);
//  AddProp('Reg.XmlText', TaskDef.RegistrationInfo.XmlText); //Not supported

  AddProp('AllowDemandStart', TaskDef.Settings.AllowDemandStart);
  AddProp('RestartInterval', TaskDef.Settings.RestartInterval);
  AddProp('RestartCount', TaskDef.Settings.RestartCount);
  AddProp('AllowHardTerminate', TaskDef.Settings.AllowHardTerminate);
  AddProp('Priority', TaskDef.Settings.Priority);
  AddProp('Network.Name', TaskDef.Settings.NetworkSettings.Name);
  AddProp('Network.Id', TaskDef.Settings.NetworkSettings.Id);

{ AddProp('', );
  TaskDef.Settings.MultipleInstances;
  TaskDef.Settings.StopIfGoingOnBatteries;
  TaskDef.Settings.DisallowStartIfOnBatteries;
  TaskDef.Settings.StartWhenAvailable;
  TaskDef.Settings.RunOnlyIfNetworkAvailable;
  TaskDef.Settings.ExecutionTimeLimit;
  TaskDef.Settings.DeleteExpiredTaskAfter;
  TaskDef.Settings.Compatibility;
  TaskDef.Settings.Enabled;
  TaskDef.Settings.Hidden;
  TaskDef.Settings.RunOnlyIfIdle;
  TaskDef.Settings.WakeToRun;}

  AddProp('PrincipalId', TaskDef.Principal.Id);
  AddProp('PrincipalDisplayName', TaskDef.Principal.DisplayName);
  AddProp('UserId', TaskDef.Principal.UserId);
  AddProp('GroupId', TaskDef.Principal.GroupId);
  AddProp('RunLevel', TaskRunLevelToStr(TaskDef.Principal.RunLevel));
  AddProp('LogonType', TaskLogonTypeToStr(TaskDef.Principal.LogonType));

end;

procedure TScheduledTasksMainForm.ClearProps;
begin
  mmDetails.Clear;
end;

procedure TScheduledTasksMainForm.AddProp(const AName: string; const AValue: string);
begin
  mmDetails.Lines.Add(AName + ' = ' + AValue);
end;

procedure TScheduledTasksMainForm.AddProp(const AName: string; const AValue: integer);
begin
  AddProp(AName, IntToStr(AValue));
end;

procedure TScheduledTasksMainForm.AddPropHex(const AName: string; const AValue: integer);
begin
  AddProp(AName, '0x'+IntToHex(AValue, 8));
end;

procedure TScheduledTasksMainForm.AddProp(const AName: string; const AValue: TDatetime);
begin
  AddProp(AName, DateTimeToStr(AValue));
end;

procedure TScheduledTasksMainForm.AddProp(const AName: string; const AValue: boolean);
begin
  AddProp(AName, BoolToStr(AValue, true));
end;



end.
