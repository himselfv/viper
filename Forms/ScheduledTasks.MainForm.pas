unit ScheduledTasks.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, Actions, ActnList, taskschd, StdCtrls, ExtCtrls,
  Registry;

type
  TTaskPresenceFlag = (
    tpAPI,          //Visible from API
    tpTreeCache,    //Present in TaskCache\Tree
    tpFlatCache,    //Present in TaskCache\Tasks
    tpStorage       //Present in Windows\System32\Tasks
  );
  TTaskPresenceFlags = set of TTaskPresenceFlag;
  TNdTaskData = record
    Name: string;
    Path: string;
    StateStr: string;
    FlagsStr: string;
    Source: string;
    Task: IRegisteredTask;
    GUID: string;
    Buckets: string;        //It seems that the task should have AT LEAST ONE registry-bucket (sometimes more)
    Presence: TTaskPresenceFlags;
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
    function AddOrphanTask(): PVirtualNode;
    function FindTaskByPath(const APath: string): PVirtualNode;
    procedure FindTaskByPath_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function FindTaskByGuid(const AGuid: string): PVirtualNode;
    procedure FindTaskByGuid_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
  protected
    procedure ReloadFocusedProps;
    procedure ClearProps;
    procedure AddProp(const AName: string; const AValue: string); overload;
    procedure AddProp(const AName: string; const AValue: integer); overload;
    procedure AddPropHex(const AName: string; const AValue: integer); overload;
    procedure AddProp(const AName: string; const AValue: TDatetime); overload;
    procedure AddProp(const AName: string; const AValue: boolean); overload;
  protected
    FReg: TRegistry;
    procedure LoadRegistryFlat;
    procedure LoadRegistryFlatTask(const AGuid: string);
    procedure LoadRegistryBucket(const ABucket: string);
    procedure LoadRegistryTree;
    procedure LoadRegistryTreePath(const APath: string);
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

HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\Taskcache\Tree
  The paths match the ITaskScheduler path.
  Contains a folder for every task, params:
    ID = This task's GUID (used in other registry keys).
    Index = ?? multiple tasks can have the same index in the same folder
    SD = Security descriptor? Binary. Even folders have this, not only tasks.

HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\Taskcache\Tasks
  All tasks by their GUIDs, as a flat list. Even those listed in Boot\Logon\Maintenance\Plain.
    Actions = binary
    Triggers = binary
    Hash = hash
    Path = path (allows to map ID -> Path)
    URI = path, again (can this differ from path?)
    SecurityDescriptor = security descriptor as a string
    Schema = ?? dword
    DynamicInfo = ?? binary

HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\Taskcache\*
  Boot\Logon\Maintenance\Plain -- subsets of \Tasks, by their GUIDs.
  Don't seem to have any additional info.
}
const
  sRegSchedule = '\Software\Microsoft\Windows NT\CurrentVersion\Schedule';
  sRegTasksRoot = sRegSchedule+'\Taskcache';
  sRegTasksFlat = sRegTasksRoot+'\Tasks';


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

//Task Paths and Task URIs are sometimes slightly differently formatted,
//and we need them to be the same to search by them.
function NormalizeTaskPath(const APath: string): string;
begin
  Result := APath;
  if not Result.StartsWith('\') then
    Result := '\' + Result;
end;

function TaskPresenceToStr(const AFlags: TTaskPresenceFlags): string;
begin
  Result := '';
  if tpAPI in AFlags then Result := Result + 'API ';
  if tpTreeCache in AFlags then Result := Result + 'Tree ';
  if tpFlatCache in AFlags then Result := Result + 'Flat ';
  if tpStorage in AFlags then Result := Result + 'Disk ';
end;


procedure TScheduledTasksMainForm.FormCreate(Sender: TObject);
begin
  CoInitialize(nil);
  //Sic: CLSID_TaskScheduler but IID_ITaskService.
  //Example: https://docs.microsoft.com/en-us/windows/win32/taskschd/displaying-task-names-and-state--c---
  OleCheck(CoCreateInstance(CLSID_TaskScheduler, nil, CLSCTX_INPROC_SERVER, IID_ITaskService, Self.FTaskScheduler));
  OleCheck(Self.FTaskScheduler.Connect(EmptyParam, EmptyParam, EmptyParam, EmptyParam));
  FReg := TRegistry.Create;
  FReg.RootKey := HKEY_LOCAL_MACHINE;
  FReg.Access := KEY_READ;
end;

procedure TScheduledTasksMainForm.FormDestroy(Sender: TObject);
begin
  Clear;
  FreeAndNil(FReg);
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
    4: CellText := NodeData.Source;
    5: CellText := NodeData.GUID;
    6: CellText := NodeData.Buckets;
    7: CellText := TaskPresenceToStr(NodeData.Presence);
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
    //Load tasks from COM
    Self.AddFolder('\');

    //Load tasks from registry
    Self.LoadRegistryFlat;

    Self.LoadRegistryTree;

    Self.LoadRegistryBucket('Boot');
    Self.LoadRegistryBucket('Logon');
    Self.LoadRegistryBucket('Maintenance');
    Self.LoadRegistryBucket('Plain');

    //Verify that all tasks now have GUIDs
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
  NodeData.Path := NormalizeTaskPath(ATask.Path);
  NodeData.Task := ATask;
  NodeData.StateStr := TaskStateToStr(ATask.State);
  NodeData.Presence := [tpAPI];

  NodeData.FlagsStr := '';
  if ATask.Definition.Settings.Hidden then
    NodeData.FlagsStr := NodeData.FlagsStr + 'Hidden';
  NodeData.Source := ATask.Definition.RegistrationInfo.Source;

  //Just to verify the hypothesis that Path and URI are always the same
  Assert(SameText(NodeData.Path, NormalizeTaskPath(ATask.Definition.RegistrationInfo.URI)));
end;

function TScheduledTasksMainForm.FindTaskByPath(const APath: string): PVirtualNode;
begin
  if APath='' then
    Result := nil
  else
    Result := vtTasks.IterateSubtree(nil, FindTaskByPath_Callback, pointer(APath));
end;

function TScheduledTasksMainForm.FindTaskByGuid(const AGuid: string): PVirtualNode;
begin
  if AGuid='' then
    Result := nil
  else
    Result := vtTasks.IterateSubtree(nil, FindTaskByGuid_Callback, pointer(AGuid));
end;

procedure TScheduledTasksMainForm.FindTaskByPath_Callback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Abort := SameText(Self.GetNodeData(Node).Path, string(Data));
end;

procedure TScheduledTasksMainForm.FindTaskByGuid_Callback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Abort := SameText(Self.GetNodeData(Node).GUID, string(Data));
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

  AddProp('GUID', NodeData.GUID);
  AddProp('Presence', TaskPresenceToStr(NodeData.Presence));

  Task := NodeData.Task;
  if Task = nil then begin
    AddProp('COM access', 'NO COM ACCESS');
  end else begin
    AddProp('LastRunTime', Task.LastRunTime);
    AddPropHex('LastTaskResult', Task.LastTaskResult);
    AddProp('NextRunTime', Task.NextRunTime);
    AddProp('XmlText', Task.XML);

    TaskDef := Task.Definition;
  {
    if SameText(Task.XML, TaskDef.XmlText) then
      AddProp('Def.XmlText', 'Same as XmlText')
    else
      AddProp('Def.XmlText', TaskDef.XmlText);

    Definition.XmlText is somewhat different from Task.XML:
     - It has (resourcename.dll,resourceid) strings expanded
     - It has [CDATA] blocks expanded
    I dunno the reason for this but lets print out the more raw version.
  }

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


{
The tasks are duplicated in the registry in two places,
and either one can IN THEORY contain tasks not mentioned elsewhere.

We'll scan both, find matches, and add any tasks not mentioned before.
}

procedure TScheduledTasksMainForm.LoadRegistryFlat;
var KeyNames: TStringList;
  i: integer;
begin
  if not FReg.OpenKey(sRegTasksFlat, false) then
    raise Exception.Create('Cannot open '+sRegTasksFlat);

  KeyNames := TStringList.Create;
  try
    FReg.GetKeyNames(KeyNames);
    for i := 0 to KeyNames.Count-1 do
      LoadRegistryFlatTask(KeyNames[i]);
  finally
    FreeAndNil(KeyNames);
  end;
end;

//Adds a task that hasn't been mentioned in some of the lists but has been in others
function TScheduledTasksMainForm.AddOrphanTask(): PVirtualNode;
var ATaskData: PNdTaskData;
begin
  Result := vtTasks.AddChild(nil);
  vtTasks.ReinitNode(Result, false);
  ATaskData := Self.GetNodeData(Result);
  ATaskData.FlagsStr := 'ORPHAN';
end;

procedure TScheduledTasksMainForm.LoadRegistryFlatTask(const AGuid: string);
var APath, AURI: string;
  ATaskNode: PVirtualNode;
  ATaskData: PNdTaskData;
begin
  if not FReg.OpenKey(sRegTasksFlat+'\'+AGuid, false) then
    raise Exception.Create('Cannot open '+sRegTasksFlat+'\'+AGuid);

  APath := NormalizeTaskPath(FReg.ReadString('Path'));
  AURI := NormalizeTaskPath(FReg.ReadString('URI'));
  Assert(SameText(APath, AURI), 'Path='+APath+', URI='+AURI);

  //Find the task by its path
  ATaskNode := Self.FindTaskByPath(APath);
  if ATaskNode <> nil then
    ATaskData := Self.GetNodeData(ATaskNode)
  else begin
    //Have to create a task
    ATaskNode := Self.AddOrphanTask();
    ATaskData := Self.GetNodeData(ATaskNode);
    ATaskData.Name := ExtractFilename(APath); //The last component is a name
    ATaskData.Path := APath;
  end;

  ATaskData.Presence := ATaskData.Presence + [tpFlatCache];

  //Both this and tree can read GUIDs. We are called first, but if by any chance
  //there's already a GUID, let's verify that it matches.
  //We might need to handle this more gracefully if this actually happens in RL
  //(different path<->guid matches in different parts of the cache),
  //though I don't think we should expect this.
  if ATaskData.GUID <> '' then
    Assert(SameText(ATaskData.GUID, AGuid))
  else
    ATaskData.GUID := AGuid;

  //TODO: Can read additional properties here
end;

//Call only after loading GUIDs for all tasks
procedure TScheduledTasksMainForm.LoadRegistryBucket(const ABucket: string);
var KeyNames: TStringList;
  i: integer;
  TaskNode: PVirtualNode;
  TaskData: PNdTaskData;
begin
  if not FReg.OpenKey(sRegTasksRoot+'\'+ABucket, false) then
    raise Exception.Create('Cannot open '+sRegTasksRoot+'\'+ABucket);

  KeyNames := TStringList.Create;
  try
    FReg.GetKeyNames(KeyNames);
    for i := 0 to KeyNames.Count-1 do begin
      Assert(KeyNames[i]<>''); //Guid must not be empty
      TaskNode := Self.FindTaskByGuid(KeyNames[i]);
      if TaskNode = nil then begin
        TaskNode := Self.AddOrphanTask();
        TaskData := Self.GetNodeData(TaskNode);
        TaskData.GUID := KeyNames[i];
      end else
        TaskData := Self.GetNodeData(TaskNode);
      TaskData.Buckets := TaskData.Buckets + ABucket + ' ';
    end;
  finally
    FreeAndNil(KeyNames);
  end;
end;

procedure TScheduledTasksMainForm.LoadRegistryTree;
begin
  LoadRegistryTreePath('');
end;

//APath starts with \ (unless it's root) and does not end with \.
procedure TScheduledTasksMainForm.LoadRegistryTreePath(const APath: string);
var KeyNames: TStringList;
  i: integer;
  ATaskPath: string;
  TaskNode: PVirtualNode;
  TaskData: PNdTaskData;
  AGuid: string;
begin
  if not FReg.OpenKey(sRegTasksRoot+'\Tree'+APath, false) then
    raise Exception.Create('Cannot open '+sRegTasksRoot+'\Tree');

  KeyNames := TStringList.Create;
  try
    FReg.GetKeyNames(KeyNames);
    for i := 0 to KeyNames.Count-1 do begin
      ATaskPath := APath+'\'+KeyNames[i];
      FReg.OpenKey(sRegTasksRoot+'\Tree'+ATaskPath, false);

      {
      Registry folders represent either Folders or Tasks.
      Both have "SD", but only Tasks have "Id".
      TODO:
      For now I assume that "no Id" means "Folder", but we probably should verify
      this against the list of folders taken from API:
      - "no Id" entries which aren't folders in the API sense
      }

      AGuid := FReg.ReadString('Id');
      if AGuid = '' then begin
        //Assume that it's a folder
        LoadRegistryTreePath(ATaskPath);
        continue;
      end;

      //Otherwise it's a task
      //I'll leave this assertion here for now:
      Assert(AGuid<>'', 'Tree-cached task GUID empty: Path='+ATaskPath);
      //Guid must not be empty (might need graceful handling here if it happens)

      ATaskPath := NormalizeTaskPath(ATaskPath); //just in case, for comparisons
      TaskNode := Self.FindTaskByPath(ATaskPath);
      if TaskNode = nil then begin
        //Before adding an orphan, check that we have nothing by that GUID
        TaskNode := Self.FindTaskByGuid(AGuid);
        Assert(TaskNode=nil, 'Tree-cached task not found by path but known by GUID: Path='+ATaskPath+', GUID='+AGuid);
        TaskNode := Self.AddOrphanTask();
        TaskData := Self.GetNodeData(TaskNode);
        TaskData.Path := ATaskPath;
        TaskData.Name := ExtractFilename(TaskData.Path);
        TaskData.GUID := AGuid; //We know the GUID
      end else
        TaskData := Self.GetNodeData(TaskNode);
      TaskData.Presence := TaskData.Presence + [tpTreeCache];

      //Check that GUID here matches what we have
      if TaskData.GUID<>'' then //otherwise it had no Flat presentation, by itself a problem
        Assert(SameText(TaskData.GUID, AGuid), 'Tree-cached task found by path but GUID differs: Path='+ATaskPath+', GUID='+AGuid+', known GUID='+TaskData.GUID);
    end;
  finally
    FreeAndNil(KeyNames);
  end;
end;



end.
