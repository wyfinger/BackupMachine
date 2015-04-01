unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DirMon, IniFiles, Registry, process, CoolTrayIcon,
  Menus, ShellAPI;

type
  TfrmMain = class(TForm)
    mmoLog: TMemo;
    tmrArchive: TTimer;
    DirMon: TDirMon;
    Process: TProcess;
    TrayIcon: TCoolTrayIcon;
    pmTray: TPopupMenu;
    miShowHide: TMenuItem;
    miSep1: TMenuItem;
    miExit: TMenuItem;
    miConfig: TMenuItem;
    procedure DirMonCreated(Sender: TObject; FileName: String);
    procedure DirMonDeleted(Sender: TObject; FileName: String);
    procedure DirMonModified(Sender: TObject; FileName: String);
    procedure DirMonRenamed(Sender: TObject; fromFileName,
      toFileName: String);
    procedure FormCreate(Sender: TObject);
    procedure mmoLogChange(Sender: TObject);
    procedure tmrArchiveTimer(Sender: TObject);
    procedure ProcessFinished(Sender: TObject; ExitCode: Cardinal);
    procedure miShowHideClick(Sender: TObject);
    procedure mmoLogMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrayIconStartup(Sender: TObject; var ShowMainForm: Boolean);
    procedure miExitClick(Sender: TObject);
    procedure miConfigClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mmoLogEnter(Sender: TObject);
  protected
    { Protected declarations }
    FSettings  : TIniFile;
    FFilesList : TStringList;
    FLastFile  : string;
  private
    { Private declarations }
    procedure CheckAutostart(Autostart: Boolean);
    function GetSelfVersion(): string;
    function DirectoryExistsEx(Directory: string): Boolean;
  public
    { Public declarations }
    FFilesFolder    : string;   // ��� ������������
    FArchivesFolder : string;   // ���� ������������
    FRarPath        : string;   // ���� � RAR
    FCommand        : string;   // ���������
    FPeriod         : Integer;  // ����������� �������������
    FAutostart      : Boolean;
    FShowRar        : Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  tAutostart, tShowRar : Integer;
begin
 // �������� ���������� �� ����������������� �����
 FSettings := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'BMachine.ini');
 FFilesFolder    := FSettings.ReadString('path', 'files_folder', '');
 FArchivesFolder := FSettings.ReadString('path', 'archives_folder', '');
 FRarPath        := FSettings.ReadString('system', 'rar_path', '');
 FCommand        := FSettings.ReadString('system', 'command', '');
 FCommand        := StringReplace(FCommand, '%FF%', FFilesFolder, [rfReplaceAll]);
 FCommand        := StringReplace(FCommand, '%AF%', FArchivesFolder, [rfReplaceAll]);
 FPeriod         := FSettings.ReadInteger('system', 'period', 5)*1000*60;  // ������
 tAutostart      := FSettings.ReadInteger('system', 'autostart', 1);
 FAutostart      := tAutostart <> 0;
 tShowRar        := FSettings.ReadInteger('system', 'show_rar_mode', 0);
 FShowRar        := tShowRar <> 0;

 if FShowRar then Process.ShowWindow := swShowNormal
   else Process.ShowWindow := swHide;

 CheckAutostart(FAutostart);

 if FileExists(FRarPath) and
    DirectoryExists(FFilesFolder) and
    DirectoryExistsEx(FArchivesFolder) then
      begin
        tmrArchive.Interval := FPeriod;
        tmrArchive.Enabled := True;
        DirMon.Path := FFilesFolder;
        DirMon.Active := True;
      end
 else begin
   mmoLog.Lines.Add('������: ������ � �������, �������� ���� �� ����������. ���������� �� �������!');
 end;
 FFilesList := TStringList.Create;

 Left := Screen.WorkAreaRect.Right - Width;
 Top := Screen.WorkAreaRect.Bottom - Height;

 Caption := Caption + ' ' +GetSelfVersion();
end;

procedure TfrmMain.CheckAutostart(Autostart: Boolean);
begin
 with TRegistry.Create do
   try
     RootKey := HKEY_CURRENT_USER;
     if OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run', true) then
       if Autostart then WriteString('BackupMachine', Application.ExeName)
         else DeleteValue('BackupMachine');
     CloseKey;
   finally
     Free;
   end;
end;

function TfrmMain.GetSelfVersion(): string;
var
  tPath : string;
  tSize : Cardinal;
  tLen  : Cardinal;
  tBuff : PChar;
  tValue: PChar;
begin
 tPath := Application.ExeName;
 tSize := GetFileVersionInfoSize(PChar(tPath), tSize);
 if tSize > 0 then
   begin
     tBuff := AllocMem(tSize);
     if GetFileVersionInfo(PChar(tPath), 0, tSize, tBuff) then
       if VerQueryValue(tBuff, PChar('StringFileInfo\041904E3\FileVersion'), Pointer(tValue), tLen) then
         Result := tValue;
     FreeMem(tBuff, tSize);
   end;
end;

// �������� ������������� ����� � ��� ����� � � ����
function TfrmMain.DirectoryExistsEx(Directory: string): Boolean;
var
  sss : TWIN32FindData;
  f   : THandle;
  p   : string;
begin
 p := ExcludeTrailingBackslash(Directory);
 f := FindFirstFile(PChar(p), sss);
 if f <> INVALID_HANDLE_VALUE then
   begin
     Result := True;
     Windows.FindClose(f);
   end;          
end;

procedure TfrmMain.DirMonCreated(Sender: TObject; FileName: String);
begin
 if (FileName <> FLastFile) and not DirectoryExists(FFilesFolder+FileName) then
   begin
     mmoLog.Lines.Add('�������� �����: "'+FileName+'"');
     FFilesList.Add(FFilesFolder+FileName);
     FLastFile := FileName;
   end;
end;

procedure TfrmMain.DirMonDeleted(Sender: TObject; FileName: String);
begin
 if not DirectoryExists(FileName) then
   mmoLog.Lines.Add('�������� �����: "'+FileName+'"');
end;

procedure TfrmMain.DirMonModified(Sender: TObject; FileName: String);
begin
 if (FileName <> FLastFile) and not DirectoryExists(FFilesFolder+FileName) then
   begin
     mmoLog.Lines.Add('��������� �����: "'+FileName+'"');
     FFilesList.Add(FFilesFolder+FileName);
     FLastFile := FileName;
   end;
end;

procedure TfrmMain.DirMonRenamed(Sender: TObject; fromFileName,
  toFileName: String);
begin
 if (toFileName <> FLastFile) and not DirectoryExists(FFilesFolder+toFileName) then
   begin
     mmoLog.Lines.Add('�������������� �����: "'+fromFileName+'" � "'+toFileName+'"');
     FFilesList.Add(FFilesFolder+toFileName);
     FLastFile := toFileName;
   end;
end;

procedure TfrmMain.mmoLogChange(Sender: TObject);
begin
 mmoLog.ScrollBy(0, 1000);
 HideCaret(mmoLog.Handle);
end;

procedure TfrmMain.tmrArchiveTimer(Sender: TObject);
var
  DateStr: string;
begin
 if (FFilesList.Count > 0) and not Process.Active then
   begin
     DateTimeToString(DateStr, 'hh:mm:ss', Now());
     mmoLog.Lines.Add(DateStr + ' ������ �������������....');
     FFilesList.SaveToFile(FArchivesFolder+'files.lst');
     FFilesList.Clear;
     Process.AppName := FRarPath;
     Process.CommandLine := FCommand;
     Process.Directory := FArchivesFolder;
     Process.Execute;
     FLastFile := '';
   end;
end;

procedure TfrmMain.ProcessFinished(Sender: TObject; ExitCode: Cardinal);
var
  DateStr : string;
  Msg     : string;
begin
 DateTimeToString(DateStr, 'hh:mm:ss', Now());
 if ExitCode = 0 then
   mmoLog.Lines.Add(DateStr + ' ....��������� �������������')
 else begin
   case ExitCode of
       1 : Msg := '������������� ������.';
       2 : Msg := '����������� ������.';
       3 : Msg := '�������� ����������� �����, ������ ����������.';
       4 : Msg := '������� �������� �����, ��������������� �������� ''k''.';
       5 : Msg := '������ ������ �� ����.';
       6 : Msg := '������ �������� �����.';
       7 : Msg := '�������� �������� � ��������� ������.';
       8 : Msg := '������������ ������ ��� ���������� ��������.';
       9 : Msg := '������ �������� �����.';
      10 : Msg := '��� ������, ��������������� ��������� �����, � ����������.';
      11 : Msg := '�������� ������.';
     255 : Msg := '������� ���������� �������������.';
   end;
   mmoLog.Lines.Add(DateStr + ' ....������: ' + Msg);
 end; 
end;

procedure TfrmMain.miShowHideClick(Sender: TObject);
begin
 frmMain.Visible := not frmMain.Visible;
 if frmMain.Visible then miShowHide.Caption := '������ ���'
   else miShowHide.Caption := '�������� ���';
end;

procedure TfrmMain.mmoLogMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 HideCaret(mmoLog.Handle);
end;

procedure TfrmMain.mmoLogEnter(Sender: TObject);
begin
 HideCaret(mmoLog.Handle);
end;

procedure TfrmMain.TrayIconStartup(Sender: TObject;
  var ShowMainForm: Boolean);
begin
 ShowMainForm := false;
 HideCaret(mmoLog.Handle);
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TfrmMain.miConfigClick(Sender: TObject);
var
  cmd : string;
begin
 cmd := ExtractFilePath(Application.ExeName) + 'BMachine.ini';
 ShellExecute(handle, 'open', PChar(cmd), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 TrayIcon.HideMainForm;
 CanClose := False;
end;

end.
