unit taskschd;

interface

uses
  Windows, ActiveX;

const
  TaskSchedulerMajorVersion = 2;
  TaskSchedulerMinorVersion = 0;

  IID_IAction: TGUID = '{BAE54997-48B1-4CBE-9965-D6BE263EBEA4}';
  IID_IActionCollection: TGUID = '{02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}';
  IID_IBootTrigger: TGUID = '{2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}';
  IID_IComHandlerAction: TGUID = '{6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}';
  IID_IDailyTrigger: TGUID = '{126C5CD8-B288-41D5-8DBF-E491446ADC5C}';
  IID_IEmailAction: TGUID = '{10F62C64-7E16-4314-A0C2-0C3683F99D40}';
  IID_IEventTrigger: TGUID = '{D45B0167-9653-4EEF-B94F-0732CA7AF251}';
  IID_IExecAction: TGUID = '{4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}';
  IID_IIdleSettings: TGUID = '{84594461-0053-4342-A8FD-088FABF11F32}';
  IID_IIdleTrigger: TGUID = '{D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}';
  IID_ILogonTrigger: TGUID = '{72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}';
  IID_IMonthlyDOWTrigger: TGUID = '{77D025A3-90FA-43AA-B52E-CDA5499B946A}';
  IID_IMonthlyTrigger: TGUID = '{97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}';
  IID_INetworkSettings: TGUID = '{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}';
  IID_IPrincipal: TGUID = '{D98D51E5-C9B4-496A-A9C1-18980261CF0F}';
  IID_IRegisteredTask: TGUID = '{9C86F320-DEE3-4DD1-B972-A303F26B061E}';
  IID_IRegisteredTaskCollection: TGUID = '{86627EB4-42A7-41E4-A4D9-AC33A72F2D52}';
  IID_IRegistrationInfo: TGUID = '{416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}';
  IID_IRegistrationTrigger: TGUID = '{4C8FEC3A-C218-4E0C-B23D-629024DB91A2}';
  IID_IRepetitionPattern: TGUID = '{7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}';
  IID_IRunningTask: TGUID = '{653758FB-7B9A-4F1E-A471-BEEB8E9B834E}';
  IID_IRunningTaskCollection: TGUID = '{6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}';
  IID_ISessionStateChangeTrigger: TGUID = '{754DA71B-4385-4475-9DD9-598294FA3641}';
  IID_IShowMessageAction: TGUID = '{505E9E68-AF89-46B8-A30F-56162A83D537}';
  IID_ITaskDefinition: TGUID = '{F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}';
  IID_ITaskFolder: TGUID = '{8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}';
  IID_ITaskFolderCollection: TGUID = '{79184A66-8664-423F-97F1-637356A5D812}';
  IID_ITaskHandler: TGUID = '{839D7762-5121-4009-9234-4F0D19394F04}';
  IID_ITaskHandlerStatus: TGUID = '{EAEC7A8F-27A0-4DDC-8675-14726A01A38A}';
  IID_ITaskNamedValueCollection: TGUID = '{B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}';
  IID_ITaskNamedValuePair: TGUID = '{39038068-2B46-4AFD-8662-7BB6F868D221}';
  IID_ITaskService: TGUID = '{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}';
  IID_ITaskSettings: TGUID = '{8FD4711D-2D02-4C8C-87E3-EFF699DE127E}';
  IID_ITaskSettings2: TGUID = '{2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}';
  IID_ITaskVariables: TGUID = '{3E4C9351-D966-4B8B-BB87-CEBA68BB0107}';
  IID_ITimeTrigger: TGUID = '{B45747E0-EBA7-4276-9F29-85C5BB300006}';
  IID_ITrigger: TGUID = '{09941815-EA89-4B5B-89E0-2A773801FAC3}';
  IID_ITriggerCollection: TGUID = '{85DF5081-1B24-4F32-878A-D9D14DF4CB77}';
  IID_IWeeklyTrigger: TGUID = '{5038FC98-82FF-436D-8728-A512A57C9DC1}';

  LIBID_TaskScheduler: TGUID = '{E34CB9F1-C7F7-424C-BE29-027DCC09363A}';

  CLSID_TaskHandlerStatusPS: TGUID = '{9F15266D-D7BA-48F0-93C1-E6895F6FE5AC}';
  CLSID_TaskHandlerPS: TGUID = '{F2A69DB7-DA2C-4352-9066-86FEE6DACAC9}';
  CLSID_TaskScheduler: TGUID = '{0F87369F-A4E5-4CFC-BD3E-73E6154572DD}';


  SCHED_S_TASK_READY = $00041300;   // The task is ready to run at its next scheduled time.
  SCHED_S_TASK_RUNNING = $00041301;   // The task is currently running.
  SCHED_S_TASK_DISABLED = $00041302;   // The task will not run at the scheduled times because it has been disabled.
  SCHED_S_TASK_HAS_NOT_RUN = $00041303;   // The task has not yet run.
  SCHED_S_TASK_NO_MORE_RUNS = $00041304;   // There are no more runs scheduled for this task.
  SCHED_S_TASK_NOT_SCHEDULED = $00041305;   // One or more of the properties that are needed to run this task on a schedule have not been set.
  SCHED_S_TASK_TERMINATED = $00041306;   // The last run of the task was terminated by the user.
  SCHED_S_TASK_NO_VALID_TRIGGERS = $00041307;   // Either the task has no triggers or the existing triggers are disabled or not set.
  SCHED_S_EVENT_TRIGGER = $00041308;   // Event triggers do not have set run times.
  SCHED_E_TRIGGER_NOT_FOUND = $80041309;   // A task's trigger is not found.
  SCHED_E_TASK_NOT_READY = $8004130A;   // One or more of the properties required to run this task have not been set.
  SCHED_E_TASK_NOT_RUNNING = $8004130B;   // There is no running instance of the task.
  SCHED_E_SERVICE_NOT_INSTALLED = $8004130C;   // The Task Scheduler service is not installed on this computer.
  SCHED_E_CANNOT_OPEN_TASK = $8004130D;   // The task object could not be opened.
  SCHED_E_INVALID_TASK = $8004130E;   // The object is either an invalid task object or is not a task object.
  SCHED_E_ACCOUNT_INFORMATION_NOT_SET = $8004130F;   // No account information could be found in the Task Scheduler security database for the task indicated.
  SCHED_E_ACCOUNT_NAME_NOT_FOUND = $80041310;   // Unable to establish existence of the account specified.
  SCHED_E_ACCOUNT_DBASE_CORRUPT = $80041311;   // Corruption was detected in the Task Scheduler security database; the database has been reset.
  SCHED_E_NO_SECURITY_SERVICES = $80041312;   // Task Scheduler security services are available only on Windows NT.
  SCHED_E_UNKNOWN_OBJECT_VERSION = $80041313;   // The task object version is either unsupported or invalid.
  SCHED_E_UNSUPPORTED_ACCOUNT_OPTION = $80041314;   // The task has been configured with an unsupported combination of account settings and run time options.
  SCHED_E_SERVICE_NOT_RUNNING = $80041315;   // The Task Scheduler Service is not running.
  SCHED_E_UNEXPECTEDNODE = $80041316;   // The task XML contains an unexpected node.
  SCHED_E_NAMESPACE = $80041317;   // The task XML contains an element or attribute from an unexpected namespace.
  SCHED_E_INVALIDVALUE = $80041318;   // The task XML contains a value which is incorrectly formatted or out of range.
  SCHED_E_MISSINGNODE = $80041319;   // The task XML is missing a required element or attribute.
  SCHED_E_MALFORMEDXML = $8004131A;   // The task XML is malformed.
  SCHED_S_SOME_TRIGGERS_FAILED = $0004131B;   // The task is registered, but not all specified triggers will start the task.
  SCHED_S_BATCH_LOGON_PROBLEM = $0004131C;   // The task is registered, but may fail to start. Batch logon privilege needs to be enabled for the task principal.
  SCHED_E_TOO_MANY_NODES = $8004131D;   // The task XML contains too many nodes of the same type.
  SCHED_E_PAST_END_BOUNDARY = $8004131E;   // The task cannot be started after the trigger end boundary.
  SCHED_E_ALREADY_RUNNING = $8004131F;   // An instance of this task is already running.
  SCHED_E_USER_NOT_LOGGED_ON = $80041320;   // The task will not run because the user is not logged on.
  SCHED_E_INVALID_TASK_HASH = $80041321;   // The task image is corrupt or has been tampered with.
  SCHED_E_SERVICE_NOT_AVAILABLE = $80041322;   // The Task Scheduler service is not available.
  SCHED_E_SERVICE_TOO_BUSY = $80041323;   // The Task Scheduler service is too busy to handle your request. Please try again later.
  SCHED_E_TASK_ATTEMPTED = $80041324;   // The Task Scheduler service attempted to run the task, but the task did not run due to one of the constraints in the task definition.
  SCHED_S_TASK_QUEUED = $00041325;   // The Task Scheduler service has asked the task to run.
  SCHED_E_TASK_DISABLED = $80041326;   // The task is disabled.
  SCHED_E_TASK_NOT_V1_COMPAT = $80041327;   // The task has properties that are not compatible with earlier versions of Windows.
  SCHED_E_START_ON_DEMAND = $80041328;   // The task settings do not allow the task to start on demand.

type
  TTaskRunFlags = (
    TASK_RUN_NO_FLAGS	= $0,
    TASK_RUN_AS_SELF	= $1,
    TASK_RUN_IGNORE_CONSTRAINTS	= $2,
    TASK_RUN_USE_SESSION_ID	= $4,
    TASK_RUN_USER_SID	= $8
  );

  TASK_ENUM_FLAGS = (
    TASK_ENUM_HIDDEN = $1
  );

  TASK_LOGON_TYPE = (
    TASK_LOGON_NONE = 0,
    TASK_LOGON_PASSWORD = 1,
    TASK_LOGON_S4U = 2,
    TASK_LOGON_INTERACTIVE_TOKEN = 3,
    TASK_LOGON_GROUP = 4,
    TASK_LOGON_SERVICE_ACCOUNT = 5,
    TASK_LOGON_INTERACTIVE_TOKEN_OR_PASSWORD = 6
  );

  TASK_RUNLEVEL_TYPE = (
    TASK_RUNLEVEL_LUA	= 0,
    TASK_RUNLEVEL_HIGHEST	= 1
  );

  TASK_STATE = (
    TASK_STATE_UNKNOWN = 0,
    TASK_STATE_DISABLED	= 1,
    TASK_STATE_QUEUED	= 2,
    TASK_STATE_READY	= 3,
    TASK_STATE_RUNNING	= 4
  );

  TASK_CREATION = (
    TASK_VALIDATE_ONLY = $1,
    TASK_CREATE	= $2,
    TASK_UPDATE	= $4,
    TASK_CREATE_OR_UPDATE	= $6,
    TASK_DISABLE	= $8,
    TASK_DONT_ADD_PRINCIPAL_ACE	= $10,
    TASK_IGNORE_REGISTRATION_TRIGGERS	= $20
  );

  PTASK_TRIGGER_TYPE2 = ^TASK_TRIGGER_TYPE2;
  TASK_TRIGGER_TYPE2 = (
    TASK_TRIGGER_EVENT	= 0,
    TASK_TRIGGER_TIME	= 1,
    TASK_TRIGGER_DAILY	= 2,
    TASK_TRIGGER_WEEKLY	= 3,
    TASK_TRIGGER_MONTHLY	= 4,
    TASK_TRIGGER_MONTHLYDOW	= 5,
    TASK_TRIGGER_IDLE	= 6,
    TASK_TRIGGER_REGISTRATION	= 7,
    TASK_TRIGGER_BOOT	= 8,
    TASK_TRIGGER_LOGON	= 9,
    TASK_TRIGGER_SESSION_STATE_CHANGE	= 11
  );

  TASK_SESSION_STATE_CHANGE_TYPE = (
    TASK_CONSOLE_CONNECT	= 1,
    TASK_CONSOLE_DISCONNECT	= 2,
    TASK_REMOTE_CONNECT	= 3,
    TASK_REMOTE_DISCONNECT	= 4,
    TASK_SESSION_LOCK	= 7,
    TASK_SESSION_UNLOCK	= 8
  );

  TASK_ACTION_TYPE = (
    TASK_ACTION_EXEC = 0,
    TASK_ACTION_COM_HANDLER	= 5,
    TASK_ACTION_SEND_EMAIL	= 6,
    TASK_ACTION_SHOW_MESSAGE	= 7
  );

  TASK_INSTANCES_POLICY = (
    TASK_INSTANCES_PARALLEL	= 0,
    TASK_INSTANCES_QUEUE	= 1,
    TASK_INSTANCES_IGNORE_NEW	= 2,
    TASK_INSTANCES_STOP_EXISTING	= 3
  );

  TASK_COMPATIBILITY = (
    TASK_COMPATIBILITY_AT	= 0,
    TASK_COMPATIBILITY_V1	= 1,
    TASK_COMPATIBILITY_V2	= 2
  );


  DAILY = packed record
    DaysInterval: Word;
  end;

  MONTHLYDATE = packed record
    rgfDays: DWORD;
    rgfMonths: WORD;
  end;

  MONTHLYDOW = packed record
    wWhichWeek: WORD;
    rgfDaysOfTheWeek: WORD;
    rgfMonths: WORD;
  end;

  WEEKLY = packed record
    WeeksInterval: WORD;
    rgfDaysOfTheWeek: WORD;
  end;

  TRIGGER_TYPE_UNION = record
    case Integer of
      0:
        ( Daily: DAILY; );
      1:
        ( Weekly: WEEKLY; );
      2:
        ( MonthlyDate: MONTHLYDATE; );
      3:
        ( MonthlyDOW: MONTHLYDOW; );
  end;

type
  IActionCollection = interface;
  ITaskFolderCollection = interface;

  IRepetitionPattern = interface(IDispatch)
    ['{7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}']
    function get_Interval: TBStr; safecall;
    procedure set_Interval(pInterval: TBStr); safecall;
    function get_Duration: TBStr; safecall;
    procedure set_Duration(pDuration: TBStr); safecall;
    function get_StopAtDurationEnd: WordBool; safecall;
    procedure set_StopAtDurationEnd(pStop: WordBool); safecall;
    property Interval: TBStr read get_Interval write set_Interval;
    property Duration: TBStr read get_Duration write set_Duration;
    property StopAtDurationEnd: WordBool read get_StopAtDurationEnd write set_StopAtDurationEnd;
  end;

  ITaskNamedValuePair = interface(IDispatch)
    ['{39038068-2B46-4AFD-8662-7BB6F868D221}']
    function get_Name: TBStr; safecall;
    procedure set_Name(pName: TBStr); safecall;
    function get_Value: TBStr; safecall;
    procedure set_Value(pValue: TBStr); safecall;
    property Name: TBStr read get_Name write set_Name;
    property Value: TBStr read get_Value write set_Value;
  end;

  ITaskNamedValueCollection = interface(IDispatch)
    ['{B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}']
    function get_Count: Integer; safecall;
    function get_Item(index: Integer): ITaskNamedValuePair; safecall;
    function get__NewEnum: IUnknown; safecall;
    function Create(Name: TBStr; Value: TBStr; out ppPair: ITaskNamedValuePair): HRESULT; stdcall;
    function Remove(index: LONG): HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
    property Count: Integer read get_Count;
    property Item[index: Integer]: ITaskNamedValuePair read get_Item; default;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

  IRegistrationInfo = interface(IDispatch)
    ['{416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}']
    function get_Description: TBStr; safecall;
    procedure set_Description(pDescription: TBStr); safecall;
    function get_Author: TBStr; safecall;
    procedure set_Author(pAuthor: TBStr); safecall;
    function get_Version: TBStr; safecall;
    procedure set_Version(pVersion: TBStr); safecall;
    function get_Date: TBStr; safecall;
    procedure set_Date(pDate: TBStr); safecall;
    function get_Documentation: TBStr; safecall;
    procedure set_Documentation(pDocumentation: TBStr); safecall;
    function get_XmlText: TBStr; safecall;
    procedure set_XmlText(pText: TBStr); safecall;
    function get_URI: TBStr; safecall;
    procedure set_URI(pUri: TBStr); safecall;
    function get_SecurityDescriptor: OleVariant; safecall;
    procedure set_SecurityDescriptor(pSddl: OleVariant); safecall;
    function get_Source: TBStr; safecall;
    procedure set_Source(pSource: TBStr); safecall;
    property Description: TBStr read get_Description write set_Description;
    property Author: TBStr read get_Author write set_Author;
    property Version: TBStr read get_Version write set_Version;
    property Date: TBStr read get_Date write set_Date;
    property Documentation: TBStr read get_Documentation write set_Documentation;
    property XmlText: TBStr read get_XmlText write set_XmlText;
    property URI: TBStr read get_URI write set_URI;
    property SecurityDescriptor: OleVariant read get_SecurityDescriptor write set_SecurityDescriptor;
    property Source: TBStr read get_Source write set_Source;
  end;

  ITrigger = interface(IDispatch)
    ['{09941815-EA89-4B5B-89E0-2A773801FAC3}']
    function get_type: TASK_TRIGGER_TYPE2; safecall;
    function get_Id: TBStr; safecall;
    procedure set_Id(pId: TBStr); safecall;
    function get_Repetition: IRepetitionPattern; safecall;
    procedure set_Repetition(ppRepeat: IRepetitionPattern); safecall;
    function get_ExecutionTimeLimit: TBStr; safecall;
    procedure set_ExecutionTimeLimit(pTimeLimit: TBStr); safecall;
    function get_StartBoundary: TBStr; safecall;
    procedure set_StartBoundary(pStart: TBStr); safecall;
    function get_EndBoundary: TBStr; safecall;
    procedure set_EndBoundary(pEnd: TBStr); safecall;
    function get_Enabled: WordBool; safecall;
    procedure set_Enabled(pEnabled: WordBool); safecall;
    property TriggerType: TASK_TRIGGER_TYPE2 read get_type;
    property Id: TBStr read get_Id write set_Id;
    property Repetition: IRepetitionPattern read get_Repetition write set_Repetition;
    property ExecutionTimeLimit: TBStr read get_ExecutionTimeLimit write set_ExecutionTimeLimit;
    property StartBoundary: TBStr read get_StartBoundary write set_StartBoundary;
    property EndBoundary: TBStr read get_EndBoundary write set_EndBoundary;
    property Enabled: WordBool read get_Enabled write set_Enabled;
  end;

  IBootTrigger = interface(ITrigger)
    ['{2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}']
    function get_Delay: TBStr; safecall;
    procedure set_Delay(pDelay: TBStr); safecall;
    property Delay: TBStr read get_Delay write set_Delay;
  end;

  IDailyTrigger = interface(ITrigger)
    ['{126C5CD8-B288-41D5-8DBF-E491446ADC5C}']
    function get_DaysInterval: Smallint; safecall;
    procedure set_DaysInterval(pDays: Smallint); safecall;
    function get_RandomDelay: TBStr; safecall;
    procedure set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysInterval: Smallint read get_DaysInterval write set_DaysInterval;
    property RandomDelay: TBStr read get_RandomDelay write set_RandomDelay;
  end;

  IEventTrigger = interface(ITrigger)
    ['{D45B0167-9653-4EEF-B94F-0732CA7AF251}']
    function get_Subscription: TBStr; safecall;
    procedure set_Subscription(pQuery: TBStr); safecall;
    function get_Delay: TBStr; safecall;
    procedure set_Delay(pDelay: TBStr); safecall;
    function get_ValueQueries: ITaskNamedValueCollection; safecall;
    procedure set_ValueQueries(ppNamedXPaths: ITaskNamedValueCollection); safecall;
    property Subscription: TBStr read get_Subscription write set_Subscription;
    property Delay: TBStr read get_Delay write set_Delay;
    property ValueQueries: ITaskNamedValueCollection read get_ValueQueries write set_ValueQueries;
  end;

  IIdleTrigger = interface(ITrigger)
    ['{D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}']
  end;

  ILogonTrigger = interface(ITrigger)
    ['{72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}']
    function get_Delay: TBStr; safecall;
    procedure set_Delay(pDelay: TBStr); safecall;
    function get_UserId: TBStr; safecall;
    procedure set_UserId(pUser: TBStr); safecall;
    property Delay: TBStr read get_Delay write set_Delay;
    property UserId: TBStr read get_UserId write set_UserId;
  end;

  IMonthlyTrigger = interface(ITrigger)
    ['{97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}']
    function get_DaysOfMonth: Integer; safecall;
    procedure set_DaysOfMonth(pDays: Integer); safecall;
    function get_MonthsOfYear: Smallint; safecall;
    procedure set_MonthsOfYear(pMonths: Smallint); safecall;
    function get_RunOnLastDayOfMonth: WordBool; safecall;
    procedure set_RunOnLastDayOfMonth(pLastDay: WordBool); safecall;
    function get_RandomDelay: TBStr; safecall;
    procedure set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysOfMonth: Integer read get_DaysOfMonth write set_DaysOfMonth;
    property MonthsOfYear: Smallint read get_MonthsOfYear write set_MonthsOfYear;
    property RunOnLastDayOfMonth: WordBool read get_RunOnLastDayOfMonth write set_RunOnLastDayOfMonth;
    property RandomDelay: TBStr read get_RandomDelay write set_RandomDelay;
  end;

  IMonthlyDOWTrigger = interface(ITrigger)
    ['{77D025A3-90FA-43AA-B52E-CDA5499B946A}']
    function get_DaysOfWeek: Smallint; safecall;
    procedure set_DaysOfWeek(pDays: Smallint); safecall;
    function get_WeeksOfMonth: Smallint; safecall;
    procedure set_WeeksOfMonth(pWeeks: Smallint); safecall;
    function get_MonthsOfYear: Smallint; safecall;
    procedure set_MonthsOfYear(pMonths: Smallint); safecall;
    function get_RunOnLastWeekOfMonth: WordBool; safecall;
    procedure set_RunOnLastWeekOfMonth(pLastWeek: WordBool); safecall;
    function get_RandomDelay: TBStr; safecall;
    procedure set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysOfWeek: Smallint read get_DaysOfWeek write set_DaysOfWeek;
    property WeeksOfMonth: Smallint read get_WeeksOfMonth write set_WeeksOfMonth;
    property MonthsOfYear: Smallint read get_MonthsOfYear write set_MonthsOfYear;
    property RunOnLastWeekOfMonth: WordBool read get_RunOnLastWeekOfMonth write set_RunOnLastWeekOfMonth;
    property RandomDelay: TBStr read get_RandomDelay write set_RandomDelay;
  end;

  IRegistrationTrigger = interface(ITrigger)
    ['{4C8FEC3A-C218-4E0C-B23D-629024DB91A2}']
    function get_Delay: TBStr; safecall;
    procedure set_Delay(pDelay: TBStr); safecall;
    property Delay: TBStr read get_Delay write set_Delay;
  end;

  ISessionStateChangeTrigger = interface(ITrigger)
    ['{754DA71B-4385-4475-9DD9-598294FA3641}']
    function get_Delay: TBStr; safecall;
    procedure set_Delay(pDelay: TBStr); safecall;
    function get_UserId: TBStr; safecall;
    procedure set_UserId(pUser: TBStr); safecall;
    function get_StateChange: TASK_SESSION_STATE_CHANGE_TYPE; safecall;
    procedure set_StateChange(pType: TASK_SESSION_STATE_CHANGE_TYPE); safecall;
    property Delay: TBStr read get_Delay write set_Delay;
    property UserId: TBStr read get_UserId write set_UserId;
    property StateChange: TASK_SESSION_STATE_CHANGE_TYPE read get_StateChange write set_StateChange;
  end;

  ITimeTrigger = interface(ITrigger)
    ['{B45747E0-EBA7-4276-9F29-85C5BB300006}']
    function get_RandomDelay: TBStr; safecall;
    procedure set_RandomDelay(pRandomDelay: TBStr); safecall;
    property RandomDelay: TBStr read get_RandomDelay write set_RandomDelay;
  end;

  IWeeklyTrigger = interface(ITrigger)
    ['{5038FC98-82FF-436D-8728-A512A57C9DC1}']
    function get_DaysOfWeek: Smallint; safecall;
    procedure set_DaysOfWeek(pDays: Smallint); safecall;
    function get_WeeksInterval: Smallint; safecall;
    procedure set_WeeksInterval(pWeeks: Smallint); safecall;
    function get_RandomDelay: TBStr; safecall;
    procedure set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysOfWeek: Smallint read get_DaysOfWeek write set_DaysOfWeek;
    property WeeksInterval: Smallint read get_WeeksInterval write set_WeeksInterval;
    property RandomDelay: TBStr read get_RandomDelay write set_RandomDelay;
  end;

  ITriggerCollection = interface(IDispatch)
    ['{85DF5081-1B24-4F32-878A-D9D14DF4CB77}']
    function get_Count: Integer; safecall;
    function get_Item(index: Integer): ITrigger; safecall;
    function get__NewEnum: IUnknown; safecall;
    function Create(triggerType: TASK_TRIGGER_TYPE2; out ppTrigger: ITrigger): HRESULT; stdcall;
    function Remove(index: OleVariant): HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
    property Count: Integer read get_Count;
    property Item[index: Integer]: ITrigger read get_Item; default;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

  IIdleSettings = interface(IDispatch)
    ['{84594461-0053-4342-A8FD-088FABF11F32}']
    function get_IdleDuration: TBStr; safecall;
    procedure set_IdleDuration(pDelay: TBStr); safecall;
    function get_WaitTimeout: TBStr; safecall;
    procedure set_WaitTimeout(pTimeout: TBStr); safecall;
    function get_StopOnIdleEnd: WordBool; safecall;
    procedure set_StopOnIdleEnd(pStop: WordBool); safecall;
    function get_RestartOnIdle: WordBool; safecall;
    procedure set_RestartOnIdle(pRestart: WordBool); safecall;
    property IdleDuration: TBStr read get_IdleDuration write set_IdleDuration;
    property WaitTimeout: TBStr read get_WaitTimeout write set_WaitTimeout;
    property StopOnIdleEnd: WordBool read get_StopOnIdleEnd write set_StopOnIdleEnd;
    property RestartOnIdle: WordBool read get_RestartOnIdle write set_RestartOnIdle;
  end;

  INetworkSettings = interface(IDispatch)
    ['{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}']
    function get_Name: TBStr; safecall;
    procedure set_Name(pName: TBStr); safecall;
    function get_Id: TBStr; safecall;
    procedure set_Id(pId: TBStr); safecall;
    property Name: TBStr read get_Name write set_Name;
    property Id: TBStr read get_Id write set_Id;
  end;

  ITaskSettings = interface(IDispatch)
    ['{8FD4711D-2D02-4C8C-87E3-EFF699DE127E}']
    function get_AllowDemandStart: WordBool; safecall;
    procedure set_AllowDemandStart(pAllowDemandStart: WordBool); safecall;
    function get_RestartInterval: TBStr; safecall;
    procedure set_RestartInterval(pRestartInterval: TBStr); safecall;
    function get_RestartCount: SYSINT; safecall;
    procedure set_RestartCount(pRestartCount: SYSINT); safecall;
    function get_MultipleInstances: TASK_INSTANCES_POLICY; safecall;
    procedure set_MultipleInstances(pPolicy: TASK_INSTANCES_POLICY); safecall;
    function get_StopIfGoingOnBatteries: WordBool; safecall;
    procedure set_StopIfGoingOnBatteries(pStopIfOnBatteries: WordBool); safecall;
    function get_DisallowStartIfOnBatteries: WordBool; safecall;
    procedure set_DisallowStartIfOnBatteries(pDisallowStart: WordBool); safecall;
    function get_AllowHardTerminate: WordBool; safecall;
    procedure set_AllowHardTerminate(pAllowHardTerminate: WordBool); safecall;
    function get_StartWhenAvailable: WordBool; safecall;
    procedure set_StartWhenAvailable(pStartWhenAvailable: WordBool); safecall;
    function get_XmlText: TBStr; safecall;
    procedure set_XmlText(pText: TBStr); safecall;
    function get_RunOnlyIfNetworkAvailable: WordBool; safecall;
    procedure set_RunOnlyIfNetworkAvailable(pRunOnlyIfNetworkAvailable: WordBool); safecall;
    function get_ExecutionTimeLimit: TBStr; safecall;
    procedure set_ExecutionTimeLimit(pExecutionTimeLimit: TBStr); safecall;
    function get_Enabled: WordBool; safecall;
    procedure set_Enabled(pEnabled: WordBool); safecall;
    function get_DeleteExpiredTaskAfter: TBStr; safecall;
    procedure set_DeleteExpiredTaskAfter(pExpirationDelay: TBStr); safecall;
    function get_Priority: SYSINT; safecall;
    procedure set_Priority(pPriority: SYSINT); safecall;
    function get_Compatibility: TASK_COMPATIBILITY; safecall;
    procedure set_Compatibility(pCompatLevel: TASK_COMPATIBILITY); safecall;
    function get_Hidden: WordBool; safecall;
    procedure set_Hidden(pHidden: WordBool); safecall;
    function get_IdleSettings: IIdleSettings; safecall;
    procedure set_IdleSettings(ppIdleSettings: IIdleSettings); safecall;
    function get_RunOnlyIfIdle: WordBool; safecall;
    procedure set_RunOnlyIfIdle(pRunOnlyIfIdle: WordBool); safecall;
    function get_WakeToRun: WordBool; safecall;
    procedure set_WakeToRun(pWake: WordBool); safecall;
    function get_NetworkSettings: INetworkSettings; safecall;
    procedure set_NetworkSettings(ppNetworkSettings: INetworkSettings); safecall;
    property AllowDemandStart: WordBool read get_AllowDemandStart write set_AllowDemandStart;
    property RestartInterval: TBStr read get_RestartInterval write set_RestartInterval;
    property RestartCount: SYSINT read get_RestartCount write set_RestartCount;
    property MultipleInstances: TASK_INSTANCES_POLICY read get_MultipleInstances write set_MultipleInstances;
    property StopIfGoingOnBatteries: WordBool read get_StopIfGoingOnBatteries write set_StopIfGoingOnBatteries;
    property DisallowStartIfOnBatteries: WordBool read get_DisallowStartIfOnBatteries write set_DisallowStartIfOnBatteries;
    property AllowHardTerminate: WordBool read get_AllowHardTerminate write set_AllowHardTerminate;
    property StartWhenAvailable: WordBool read get_StartWhenAvailable write set_StartWhenAvailable;
    property XmlText: TBStr read get_XmlText write set_XmlText;
    property RunOnlyIfNetworkAvailable: WordBool read get_RunOnlyIfNetworkAvailable write set_RunOnlyIfNetworkAvailable;
    property ExecutionTimeLimit: TBStr read get_ExecutionTimeLimit write set_ExecutionTimeLimit;
    property Enabled: WordBool read get_Enabled write set_Enabled;
    property DeleteExpiredTaskAfter: TBStr read get_DeleteExpiredTaskAfter write set_DeleteExpiredTaskAfter;
    property Priority: SYSINT read get_Priority write set_Priority;
    property Compatibility: TASK_COMPATIBILITY read get_Compatibility write set_Compatibility;
    property Hidden: WordBool read get_Hidden write set_Hidden;
    property IdleSettings: IIdleSettings read get_IdleSettings write set_IdleSettings;
    property RunOnlyIfIdle: WordBool read get_RunOnlyIfIdle write set_RunOnlyIfIdle;
    property WakeToRun: WordBool read get_WakeToRun write set_WakeToRun;
    property NetworkSettings: INetworkSettings read get_NetworkSettings write set_NetworkSettings;
  end;

  ITaskSettings2 = interface(IDispatch)
    ['{2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}']
    function get_DisallowStartOnRemoteAppSession: WordBool; safecall;
    procedure set_DisallowStartOnRemoteAppSession(pDisallowStart: WordBool); safecall;
    function get_UseUnifiedSchedulingEngine: WordBool; safecall;
    procedure set_UseUnifiedSchedulingEngine(pUseUnifiedEngine: WordBool); safecall;
    property DisallowStartOnRemoteAppSession: WordBool read get_DisallowStartOnRemoteAppSession write set_DisallowStartOnRemoteAppSession;
    property UseUnifiedSchedulingEngine: WordBool read get_UseUnifiedSchedulingEngine write set_UseUnifiedSchedulingEngine;
  end;

  IPrincipal = interface(IDispatch)
    ['{D98D51E5-C9B4-496A-A9C1-18980261CF0F}']
    function get_Id: TBStr; safecall;
    procedure set_Id(pId: TBStr); safecall;
    function get_DisplayName: TBStr; safecall;
    procedure set_DisplayName(pName: TBStr); safecall;
    function get_UserId: TBStr; safecall;
    procedure set_UserId(pUser: TBStr); safecall;
    function get_LogonType: TASK_LOGON_TYPE; safecall;
    procedure set_LogonType(pLogon: TASK_LOGON_TYPE); safecall;
    function get_GroupId: TBStr; safecall;
    procedure set_GroupId(pGroup: TBStr); safecall;
    function get_RunLevel: TASK_RUNLEVEL_TYPE; safecall;
    procedure set_RunLevel(pRunLevel: TASK_RUNLEVEL_TYPE); safecall;
    property Id: TBStr read get_Id write set_Id;
    property DisplayName: TBStr read get_DisplayName write set_DisplayName;
    property UserId: TBStr read get_UserId write set_UserId;
    property LogonType: TASK_LOGON_TYPE read get_LogonType write set_LogonType;
    property GroupId: TBStr read get_GroupId write set_GroupId;
    property RunLevel: TASK_RUNLEVEL_TYPE read get_RunLevel write set_RunLevel;
  end;

  IAction = interface(IDispatch)
    ['{BAE54997-48B1-4CBE-9965-D6BE263EBEA4}']
    function get_Id: TBStr; safecall;
    procedure set_Id(pId: TBStr); safecall;
    function get_type: TASK_ACTION_TYPE; safecall;
    property Id: TBStr read get_Id write set_Id;
    property ActionType: TASK_ACTION_TYPE read get_type;
  end;

  IComHandlerAction = interface(IAction)
    ['{6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}']
    function get_ClassId: TBStr; safecall;
    procedure set_ClassId(pClsid: TBStr); safecall;
    function get_Data: TBStr; safecall;
    procedure set_Data(pData: TBStr); safecall;
    property ClassId: TBStr read get_ClassId write set_ClassId;
    property Data: TBStr read get_Data write set_Data;
  end;

  IEmailAction = interface(IAction)
    ['{10F62C64-7E16-4314-A0C2-0C3683F99D40}']
    function get_Server: TBStr; safecall;
    procedure set_Server(pServer: TBStr); safecall;
    function get_Subject: TBStr; safecall;
    procedure set_Subject(pSubject: TBStr); safecall;
    function get_To_: TBStr; safecall;
    procedure set_To_(pTo: TBStr); safecall;
    function get_Cc: TBStr; safecall;
    procedure set_Cc(pCc: TBStr); safecall;
    function get_Bcc: TBStr; safecall;
    procedure set_Bcc(pBcc: TBStr); safecall;
    function get_ReplyTo: TBStr; safecall;
    procedure set_ReplyTo(pReplyTo: TBStr); safecall;
    function get_From: TBStr; safecall;
    procedure set_From(pFrom: TBStr); safecall;
    function get_HeaderFields: ITaskNamedValueCollection; safecall;
    procedure set_HeaderFields(ppHeaderFields: ITaskNamedValueCollection); safecall;
    function get_Body: TBStr; safecall;
    procedure set_Body(pBody: TBStr); safecall;
    function get_Attachments: PSafeArray; safecall;
    procedure set_Attachments(pAttachements: PSafeArray); safecall;
    property Server: TBStr read get_Server write set_Server;
    property Subject: TBStr read get_Subject write set_Subject;
    property To_: TBStr read get_To_ write set_To_;
    property Cc: TBStr read get_Cc write set_Cc;
    property Bcc: TBStr read get_Bcc write set_Bcc;
    property ReplyTo: TBStr read get_ReplyTo write set_ReplyTo;
    property From: TBStr read get_From write set_From;
    property HeaderFields: ITaskNamedValueCollection read get_HeaderFields write set_HeaderFields;
    property Body: TBStr read get_Body write set_Body;
    property Attachments: PSafeArray read get_Attachments write set_Attachments;
  end;

  IExecAction = interface(IAction)
    ['{4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}']
    function get_Path: TBStr; safecall;
    procedure set_Path(pPath: TBStr); safecall;
    function get_Arguments: TBStr; safecall;
    procedure set_Arguments(pArgument: TBStr); safecall;
    function get_WorkingDirectory: TBStr; safecall;
    procedure set_WorkingDirectory(pWorkingDirectory: TBStr); safecall;
    property Path: TBStr read get_Path write set_Path;
    property Arguments: TBStr read get_Arguments write set_Arguments;
    property WorkingDirectory: TBStr read get_WorkingDirectory write set_WorkingDirectory;
  end;

  IShowMessageAction = interface(IAction)
    ['{505E9E68-AF89-46B8-A30F-56162A83D537}']
    function get_Title: TBStr; safecall;
    procedure set_Title(pTitle: TBStr); safecall;
    function get_MessageBody: TBStr; safecall;
    procedure set_MessageBody(pMessageBody: TBStr); safecall;
    property Title: TBStr read get_Title write set_Title;
    property MessageBody: TBStr read get_MessageBody write set_MessageBody;
  end;

  IActionCollection = interface(IDispatch)
    ['{02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}']
    function get_Count: Integer; safecall;
    function get_Item(index: Integer): IAction; safecall;
    function get__NewEnum: IUnknown; safecall;
    function get_XmlText: TBStr; safecall;
    procedure set_XmlText(pText: TBStr); safecall;
    function Create(ActionType: TASK_ACTION_TYPE; out ppAction: IAction): HRESULT; stdcall;
    function Remove(index: OleVariant): HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
    function get_Context: TBStr; safecall;
    procedure set_Context(pContext: TBStr); safecall;
    property Count: Integer read get_Count;
    property Item[index: Integer]: IAction read get_Item; default;
    property _NewEnum: IUnknown read get__NewEnum;
    property XmlText: TBStr read get_XmlText write set_XmlText;
    property Context: TBStr read get_Context write set_Context;
  end;

  ITaskVariables = interface(IUnknown)
    ['{3E4C9351-D966-4B8B-BB87-CEBA68BB0107}']
    function GetInput(out pInput: TBStr): HRESULT; stdcall;
    function SetOutput(input: TBStr): HRESULT; stdcall;
    function GetContext(out pContext: TBStr): HRESULT; stdcall;
  end;

  ITaskDefinition = interface(IDispatch)
    ['{F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}']
    function get_RegistrationInfo: IRegistrationInfo; safecall;
    procedure set_RegistrationInfo(ppRegistrationInfo: IRegistrationInfo); safecall;
    function get_Triggers: ITriggerCollection; safecall;
    procedure set_Triggers(ppTriggers: ITriggerCollection); safecall;
    function get_Settings: ITaskSettings; safecall;
    procedure set_Settings(ppSettings: ITaskSettings); safecall;
    function get_Data: TBStr; safecall;
    procedure set_Data(pData: TBStr); stdcall;
    function get_Principal: IPrincipal; safecall;
    procedure set_Principal(ppPrincipal: IPrincipal); safecall;
    function get_Actions: IActionCollection; safecall;
    procedure set_Actions(ppActions: IActionCollection); safecall;
    function get_XmlText: TBStr; safecall;
    procedure set_XmlText(pXml: TBStr); safecall;
    property RegistrationInfo: IRegistrationInfo read get_RegistrationInfo write set_RegistrationInfo;
    property Triggers: ITriggerCollection read get_Triggers write set_Triggers;
    property Settings: ITaskSettings read get_Settings write set_Settings;
    property Data: TBStr read get_Data write set_Data;
    property Principal: IPrincipal read get_Principal write set_Principal;
    property Actions: IActionCollection read get_Actions write set_Actions;
    property XmlText: TBStr read get_XmlText write set_XmlText;
  end;

  IRunningTask = interface(IDispatch)
    ['{653758FB-7B9A-4F1E-A471-BEEB8E9B834E}']
    function get_Name: TBStr; safecall;
    function get_InstanceGuid: TBStr; safecall;
    function get_Path: TBStr; safecall;
    function get_State: TASK_STATE; safecall;
    function get_CurrentAction: TBStr; safecall;
    function Stop: HRESULT; stdcall;
    function Refresh: HRESULT; stdcall;
    function get_EnginePID: LongWord; safecall;
    property Name: TBStr read get_Name;
    property InstanceGuid: TBStr read get_InstanceGuid;
    property Path: TBStr read get_Path;
    property State: TASK_STATE read get_State;
    property CurrentAction: TBStr read get_CurrentAction;
    property EnginePID: LongWord read get_EnginePID;
  end;

  IRunningTaskCollection = interface(IDispatch)
    ['{6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}']
    function get_Count: Integer; safecall;
    function get_Item(index: OleVariant): IRunningTask; safecall;
    function get__NewEnum: IUnknown; safecall;
    property Count: Integer read get_Count;
    property Item[index: OleVariant]: IRunningTask read get_Item; default;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

  IRegisteredTask = interface(IDispatch)
    ['{9C86F320-DEE3-4DD1-B972-A303F26B061E}']
    function get_Name: TBStr; safecall;
    function get_Path: TBStr; safecall;
    function get_State: TASK_STATE; safecall;
    function get_Enabled: WordBool; safecall;
    procedure set_Enabled(pEnabled: WordBool); safecall;
    function Run(params: OleVariant;
      out ppRunningTask: IRunningTask): HRESULT; stdcall;
    function RunEx(params: OleVariant; flags: LONG; sessionID: Integer;
      user: TBStr; out ppRunningTask: IRunningTask): HRESULT; stdcall;
    function GetInstances(flags: LONG;
      out ppRunningTasks: IRunningTaskCollection): HRESULT; stdcall;
    function get_LastRunTime: TDateTime; safecall;
    function get_LastTaskResult: Integer; safecall;
    function get_NumberOfMissedRuns: Integer; safecall;
    function get_NextRunTime: TDateTime; safecall;
    function get_Definition: ITaskDefinition; safecall;
    function get_Xml: TBStr; safecall;
    function GetSecurityDescriptor(securityInformation: Integer; out pSddl: TBStr): HRESULT; stdcall;
    function SetSecurityDescriptor(sddl: TBStr; flags: LONG): HRESULT; stdcall;
    function Stop(flags: LONG): HRESULT; stdcall;
    function GetRunTimes(pstStart, pstEnd: PSystemTime; var pCount: DWORD;
      out pRunTimes: PSystemTime): HRESULT; stdcall;
    property Name: TBStr read get_Name;
    property Path: TBStr read get_Path;
    property State: TASK_STATE read get_State;
    property Enabled: WordBool read get_Enabled write set_Enabled;
    property LastRunTime: TDateTime read get_LastRunTime;
    property LastTaskResult: Integer read get_LastTaskResult;
    property NumberOfMissedRuns: Integer read get_NumberOfMissedRuns;
    property NextRunTime: TDateTime read get_NextRunTime;
    property Definition: ITaskDefinition read get_Definition;
    property XML: TBStr read get_Xml;
  end;

  IRegisteredTaskCollection = interface(IDispatch)
    ['{86627EB4-42A7-41E4-A4D9-AC33A72F2D52}']
    function get_Count: Integer; safecall;
    function get_Item(index: OleVariant): IRegisteredTask; safecall;
    function get__NewEnum: IUnknown; safecall;
    property Count: Integer read get_Count;
    property Item[index: OleVariant]: IRegisteredTask read get_Item; default;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

  ITaskHandler = interface(IUnknown)
    ['{839D7762-5121-4009-9234-4F0D19394F04}']
    function Start(pHandlerServices: IUnknown; data: TBStr): HRESULT; stdcall;
    function Stop(out pRetCode: HRESULT): HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
  end;

  ITaskHandlerStatus = interface(IUnknown)
    ['{EAEC7A8F-27A0-4DDC-8675-14726A01A38A}']
    function UpdateStatus(percentComplete: SHORT; statusMessage: TBStr): HRESULT; stdcall;
    function TaskCompleted(taskErrCode: HRESULT): HRESULT; stdcall;
  end;

  ITaskFolder = interface(IDispatch)
    ['{8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}']
    function get_Name: TBStr; safecall;
    function get_Path: TBStr; safecall;
    function GetFolder(Path: TBStr; out ppFolder: ITaskFolder): HRESULT; stdcall;
    function GetFolders(flags: LONG; out ppFolders: ITaskFolderCollection): HRESULT; stdcall;
    function CreateFolder(subFolderName: TBStr; sddl: OleVariant; out ppFolder: ITaskFolder): HRESULT; stdcall;
    function DeleteFolder(subFolderName: TBStr; flags: LONG): HRESULT; stdcall;
    function GetTask(path: TBStr; out ppTask: IRegisteredTask): HRESULT; stdcall;
    function GetTasks(flags: LONG; out ppTasks: IRegisteredTaskCollection): HRESULT; stdcall;
    function DeleteTask(Name: TBStr; flags: LONG): HRESULT; stdcall;
    function RegisterTask(Path: TBStr; XmlText: TBStr; flags: LONG; UserId: OleVariant;
      password: OleVariant; LogonType: TASK_LOGON_TYPE; sddl: OleVariant; out ppTask: IRegisteredTask): HRESULT; stdcall;
    function RegisterTaskDefinition(path: TBStr; pDefinition: ITaskDefinition; flags: LONG;
      TASK_CREATION: OleVariant; password: OleVariant; logonType: TASK_LOGON_TYPE;
      sddl: OleVariant; out ppTask: IRegisteredTask): HRESULT; stdcall;
    function GetSecurityDescriptor(securityInformation: Integer; out pSddl: TBStr): HRESULT; stdcall;
    function SetSecurityDescriptor(sddl: TBStr; flags: LONG): HRESULT; stdcall;
    property Name: TBStr read get_Name;
    property Path: TBStr read get_Path;
  end;

  ITaskFolderCollection = interface(IDispatch)
    ['{79184A66-8664-423F-97F1-637356A5D812}']
    function get_Count: Integer; safecall;
    function get_Item(index: OleVariant): ITaskFolder; safecall;
    function get__NewEnum: IUnknown; safecall;
    property Count: Integer read get_Count;
    property Item[index: OleVariant]: ITaskFolder read get_Item; default;
    property _NewEnum: IUnknown read get__NewEnum;
  end;

  ITaskService = interface(IDispatch)
    ['{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}']
    function GetFolder(Path: TBStr; out ppFolder: ITaskFolder): HRESULT; stdcall;
    function GetRunningTasks(flags: LONG; out ppRunningTasks: IRunningTaskCollection): HRESULT; stdcall;
    function NewTask(flags: DWORD; out ppDefinition: ITaskDefinition): HRESULT; stdcall;
    function Connect(serverName: OleVariant; user: OleVariant;
      domain: OleVariant; password: OleVariant): HRESULT; stdcall;
    function get_Connected: WordBool; safecall;
    function get_TargetServer: TBStr; safecall;
    function get_ConnectedUser: TBStr; safecall;
    function get_ConnectedDomain: TBStr; safecall;
    function get_HighestVersion: LongWord; safecall;
    property Connected: WordBool read get_Connected;
    property TargetServer: TBStr read get_TargetServer;
    property ConnectedUser: TBStr read get_ConnectedUser;
    property ConnectedDomain: TBStr read get_ConnectedDomain;
    property HighestVersion: LongWord read get_HighestVersion;
  end;

implementation

end.
