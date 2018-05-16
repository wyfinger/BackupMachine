// -----------------------------------------------------------------------------
unit process;

// -----------------------------------------------------------------------------
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  ShowWindowValues : array [ 0..11 ] of integer =
                     ( sw_Hide, sw_Maximize, sw_Minimize, sw_Restore, sw_Show,
                       sw_ShowDefault, sw_ShowMaximized, sw_ShowMinimized,
                       sw_ShowMinNoActive, sw_ShowNA, sw_ShowNoActivate,
                       sw_ShowNormal );

type
  TShowWindow = ( swHide, swMaximize, swMinimize, swRestore, swShow,
                  swShowDefault, swShowMaximized, swShowMinimized,
                  swShowMinNoActive, swShowNA, swShowNoActivate,
                  swShowNormal );

  TProcessOption = ( poNoteExit, poForceExit );
  TProcessOptions = set of TProcessOption;

  TProcessEvent = procedure ( Sender: TObject; ExitCode: LongWord ) of object;

// -----------------------------------------------------------------------------
  TProcess = class;

  TProcessThread = class( TThread )
    private
      FProcessInfo           : TProcessInformation;
      FControl               : TProcess;
    protected
      procedure ResetThread(Sender: TObject);
      procedure Execute; override;
    public
      constructor CreateThread( var dProcess: TProcess );
  end;

  TProcess = class(TComponent)
    private
      FAppName               : string;           // Process Name
      FCmdLine               : string;           // Process' Command Line
      FShowWindow            : TShowWindow;      // Process Display Option
      FDirectory             : string;           // Process' Working Dir.
      FOptions               : TProcessOptions;  // Process' Wait Options
      FIdle                  : Boolean;          // Marks Process Idle
      FThread                : TProcessThread;
      FOnFinished            : TProcessEvent;
      FOnIdle                : TNotifyEvent;
    private
      procedure DoFinished( dExitCode: LongWord );
      function IsActive : Boolean;
      procedure SetAppName(Value: string);
      procedure SetCmdLine(Value: string);
      procedure SetDirectory(Value: string);
      procedure SetShowWindow(Value: TShowWindow);
      procedure SetOptions(Values: TProcessOptions);
    public
      constructor Create( AOwner: TComponent ); override;
      destructor Destroy; override;
      procedure Execute;
      procedure ReleaseApplication;
    published
      property Active      : Boolean         read IsActive;
      property AppName     : string          read FAppName      write SetAppName;
      property CommandLine : string          read FCmdLine      write SetCmdLine;
      property Directory   : string          read FDirectory    write SetDirectory;
      property ShowWindow  : TShowWindow     read FShowWindow   write SetShowWindow;
      property WaitOptions : TProcessOptions read FOptions      write SetOptions;
      property Idle        : Boolean         read FIdle;
      property OnFinished  : TProcessEvent   read FOnFinished   write FOnFinished;
      property OnIdle      : TNotifyEvent    read FOnIdle       write FOnIdle;
  end;

// -----------------------------------------------------------------------------
procedure Register;

// -----------------------------------------------------------------------------
implementation
{ $X+ }

// -----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('Samples', [TProcess]);
end;

// -----------------------------------------------------------------------------
constructor TProcess.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FThread := nil;
  FShowWindow := swShowNormal;
  FOptions := [];
  FIdle := False;
end;

destructor TProcess.Destroy;
begin
  Self.ReleaseApplication;
  inherited Destroy;
end;

procedure TProcess.Execute;
begin
  if (not IsActive) then
  begin
    FThread := TProcessThread.CreateThread( Self );
    if (Self.WaitOptions = []) then FThread := nil;
  end;
end;

procedure TProcess.ReleaseApplication;
begin
  if (FThread <> nil) then
  try
    FThread.DoTerminate;
  except on
      E : Exception do ;
  end;
end;

procedure TProcess.DoFinished( dExitCode: LongWord );
begin
  if ((Self.WaitOptions * [poNoteExit]) <> []) then
    Self.OnFinished( Self , dExitCode );
end;

function TProcess.IsActive : Boolean;
begin
  if (Self.WaitOptions = []) then
       Result := False
  else Result := (FThread <> nil);
end;

procedure TProcess.SetAppName(Value: string);
begin
  if (not Self.IsActive) then FAppName := Value;
end;

procedure TProcess.SetCmdLine(Value: string);
begin
  if (not Self.IsActive) then FCmdLine := Value;
end;

procedure TProcess.SetDirectory(Value: string);
begin
  if (not Self.IsActive) then FDirectory := Value;
end;

procedure TProcess.SetShowWindow(Value: TShowWindow);
begin
  if (not Self.IsActive) then FShowWindow := Value;
end;

procedure TProcess.SetOptions(Values: TProcessOptions);
begin
  if (not Self.IsActive) then FOptions := Values;
end;

//------------------------------------------------------------------------------
constructor TProcessThread.CreateThread( var dProcess: TProcess );
begin
  inherited Create( True );
  FControl := dProcess;
  FreeOnTerminate := True;      // Not used due to Force Destruction
  Self.OnTerminate := Self.ResetThread;
  Resume;
end;

procedure TProcessThread.ResetThread(Sender: TObject);
var
  ovExitCode                 : LongWord;
begin
  if (FProcessInfo.hProcess <> 0) then
  begin
    GetExitCodeProcess( FProcessInfo.hProcess , ovExitCode );
    if (FProcessInfo.hProcess <> 0) then
    begin
      if ((FControl.WaitOptions * [poForceExit]) <> []) then
        TerminateProcess( FProcessInfo.hProcess, ovExitCode );
      CloseHandle( FProcessInfo.hProcess );
    end;
    if (FProcessInfo.hThread <> 0) then
      CloseHandle( FProcessInfo.hThread );
    FProcessInfo.hProcess := 0;
  end;
  FControl.FThread := nil;
end;

procedure TProcessThread.Execute;
var
  ovStartupInfo              : TStartupInfo;
  ovReturn, ovExitCode       : LongWord;
  ovCmdLine, ovDirectory     : PChar;
  ovBuildStr                 : string;
begin
  FillChar( ovStartupInfo , SizeOf( ovStartupInfo ) , 0 );
  with ovStartupInfo do
  begin
    cb := SizeOf( ovStartupInfo );
    dwFlags := startf_UseShowWindow;
    wShowWindow := ShowWindowValues[ Ord( FControl.ShowWindow ) ];
  end;
  ovExitCode := 0;
  FControl.AppName := Trim( FControl.AppName );
  FControl.CommandLine := Trim( FControl.CommandLine );
  if Length( FControl.AppName ) = 0 then
       ovBuildStr := ''
  else ovBuildStr := ( '"' + FControl.AppName + '" ' );
  ovBuildStr := ( ovBuildStr + FControl.CommandLine );
  ovCmdLine := PChar( ovBuildStr );
  FControl.Directory := Trim( FControl.Directory );
  if Length( FControl.Directory ) = 0 then
       ovDirectory := nil
  else ovDirectory := PChar( FControl.Directory );
  if CreateProcess( nil, ovCmdLine, nil, nil, False, NORMAL_PRIORITY_CLASS,
                    nil, ovDirectory, ovStartupInfo, FProcessInfo ) then
  begin
    ovReturn := WaitForInputIdle( FProcessInfo.hProcess, Infinite );
    FControl.FIdle := (ovReturn = 0);
    if Assigned( FControl.FOnIdle ) then
      FControl.FOnIdle( FControl );
    ovReturn := WAIT_TIMEOUT;
    while (ovReturn <> WAIT_OBJECT_0) and (FControl.WaitOptions <> []) do
      ovReturn := WaitForSingleObject( FProcessInfo.hProcess , 1000 );
    GetExitCodeProcess( FProcessInfo.hProcess , ovExitCode );
    if Assigned( FControl.OnFinished ) then
      FControl.DoFinished( ovExitCode );
  end;
  if (FControl.WaitOptions = []) then
  begin
    CloseHandle( FProcessInfo.hProcess );
    CloseHandle( FProcessInfo.hThread );
    FProcessInfo.hProcess := 0;
  end;
  Self.ResetThread( Self );
end;

// -----------------------------------------------------------------------------

end.
