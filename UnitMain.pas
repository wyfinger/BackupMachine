unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DirMon, IniFiles, Registry, process, CoolTrayIcon,
  Menus, ShellAPI, ImgList, ComCtrls, RichEdit, LogEdit;

type
  TfrmMain = class(TForm)
    tmrArchive: TTimer;
    DirMon: TDirMon;
    Process: TProcess;
    TrayIcon: TCoolTrayIcon;
    pmTray: TPopupMenu;
    miShowHide: TMenuItem;
    miSep1: TMenuItem;
    miExit: TMenuItem;
    miConfig: TMenuItem;
    ilState: TImageList;
    ilAnim: TImageList;
    hcLog: THeaderControl;
    btn1: TButton;
    redtLog: TLogEdit;
    procedure DirMonCreated(Sender: TObject; FileName: String);
    procedure DirMonDeleted(Sender: TObject; FileName: String);
    procedure DirMonModified(Sender: TObject; FileName: String);
    procedure DirMonRenamed(Sender: TObject; fromFileName,
      toFileName: String);
    procedure FormCreate(Sender: TObject);
    procedure tmrArchiveTimer(Sender: TObject);
    procedure ProcessFinished(Sender: TObject; ExitCode: Cardinal);
    procedure miShowHideClick(Sender: TObject);
    procedure TrayIconStartup(Sender: TObject; var ShowMainForm: Boolean);
    procedure miExitClick(Sender: TObject);
    procedure miConfigClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure hcLogSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
  protected
    { Protected declarations }
    FSettings  : TIniFile;
    FFilesList : TStringList;
    FLastFile  : string;
  private
    { Private declarations }
    procedure CheckAutostart(Autostart: Boolean);
    function  GetSelfVersion(): string;
    function  DirectoryExistsEx(Directory: string): Boolean;
    procedure AddLogLine(Level: Byte; Tag, Description: string);
  public
    { Public declarations }
    FFilesFolder    : string;   // ��� ������������
    FArchivesFolder : string;   // ���� ������������
    FRarPath        : string;   // ���� � RAR
    FCommand        : string;   // ���������
    FPeriod         : Integer;  // ����������� �������������
    FAutostart      : Boolean;
    FShowRar        : Boolean;
    FLogLevel       : Integer;

    nxLogPixels     : Integer;
    nTextFontWidth  : Integer;
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
 FLogLevel       := FSettings.ReadInteger('system', 'loglevel', 7);

 if FShowRar then Process.ShowWindow := swShowNormal
   else Process.ShowWindow := swHide;

 CheckAutostart(FAutostart);

 redtLog.TabCount := 2;
 redtLog.Tab[0] := hcLog.Sections[0].Width;
 redtLog.Tab[1] := hcLog.Sections[0].Width + hcLog.Sections[1].Width;

 if not FileExists(FRarPath) then
   AddLogLine(0, 'E', 'Can''t find RAR executable file, monitor is not started');
 if not DirectoryExists(FFilesFolder) then
   AddLogLine(0, 'E', 'Files folder is not found, monitor is not started');
 if not DirectoryExistsEx(FArchivesFolder) then
   AddLogLine(0, 'E', 'Dest folder for archives is not found, monitor is not started');

 if FileExists(FRarPath) and
    DirectoryExists(FFilesFolder) and
    DirectoryExistsEx(FArchivesFolder) then
      begin
        tmrArchive.Interval := FPeriod;
        tmrArchive.Enabled := True;
        DirMon.Path := FFilesFolder;
        DirMon.Active := True;
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
 Result := False;
 p := ExcludeTrailingBackslash(Directory);
 f := FindFirstFile(PChar(p), sss);
 if f <> INVALID_HANDLE_VALUE then Result := True;
 Windows.FindClose(f);
end;

// ���������� ������ � ���
procedure TfrmMain.AddLogLine(Level: Byte; Tag, Description: string);
var
  DateStr: string;
begin
 if not (FLogLevel and Level = Level) then Exit;
 DateTimeToString(DateStr, 'hh:mm:ss', Now());
 redtLog.SelStart := Length(redtLog.Lines.Text);
 redtLog.SelLength := 0;
 with redtLog.SelAttributes do
   begin
     if Level = 0 then  // ������
       begin
         Color := clRed;
         Style := [];
       end;
     if Level = 1 then  // ������� ��������
       begin
         Color := clWindowText;
         Style := [];
       end;
     if Level = 2 then  // ������ rar
       begin
         Color := clWindowText;
         Style := [fsBold];
       end;
   end;
 redtLog.Lines.Add('  '+ Tag +#9+ DateStr +#9+ Description);  
 redtLog.ScrollBy(0, 999);
end;

procedure TfrmMain.DirMonCreated(Sender: TObject; FileName: String);
begin
 if (FileName <> FLastFile) and not DirectoryExists(FFilesFolder+FileName) and
   FileExists(FFilesFolder+FileName) then
   begin
     AddLogLine(1, 'C', FFilesFolder+FileName);
     FFilesList.Add(FFilesFolder+FileName);
     FLastFile := FileName;
   end;
end;

procedure TfrmMain.DirMonDeleted(Sender: TObject; FileName: String);
begin
 if not DirectoryExists(FileName) then
   AddLogLine(1, 'D', FFilesFolder+FileName);
end;

procedure TfrmMain.DirMonModified(Sender: TObject; FileName: String);
begin
 if (FileName <> FLastFile) and not DirectoryExists(FFilesFolder+FileName) and
   FileExists(FFilesFolder+FileName) then
   begin
     AddLogLine(1, 'M', FFilesFolder+FileName);
     FFilesList.Add(FFilesFolder+FileName);
     FLastFile := FileName;
   end;
end;

procedure TfrmMain.DirMonRenamed(Sender: TObject; fromFileName,
  toFileName: String);
begin
 if (toFileName <> FLastFile) and not DirectoryExists(FFilesFolder+toFileName) and
   FileExists(FFilesFolder+toFileName) then
   begin
     AddLogLine(1, 'R', FFilesFolder+toFileName);
     FFilesList.Add(FFilesFolder+toFileName);
     FLastFile := toFileName;
   end;
end;

procedure TfrmMain.tmrArchiveTimer(Sender: TObject);
begin
 if (FFilesList.Count > 0) and not Process.Active then
   begin
     AddLogLine(2, 'A', 'Start RAR for archiving');
     try
       FFilesList.SaveToFile(FArchivesFolder+'files.lst');
     except
       AddLogLine(0, 'E', 'Can''t save file list in dest directory');
       Exit;
     end;
     TrayIcon.IconList := ilAnim;
     TrayIcon.CycleIcons := True;
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
 TrayIcon.IconList := ilState;
 TrayIcon.CycleIcons := False;
 TrayIcon.IconIndex := 0;
 Msg := 'RAR process is ended';
 case ExitCode of
       1 : Msg := Msg+ ' (������������� ������)';
       2 : Msg := Msg+ ' (����������� ������)';
       3 : Msg := Msg+ ' (�������� ����������� �����, ������ ����������)';
       4 : Msg := Msg+ ' (������� �������� �����, ��������������� �������� ''k'')';
       5 : Msg := Msg+ ' (������ ������ �� ����)';
       6 : Msg := Msg+ ' (������ �������� �����)';
       7 : Msg := Msg+ ' (�������� �������� � ��������� ������)';
       8 : Msg := Msg+ ' (������������ ������ ��� ���������� ��������)';
       9 : Msg := Msg+ ' (������ �������� �����)';
      10 : Msg := Msg+ ' (��� ������, ��������������� ��������� �����, � ����������)';
      11 : Msg := Msg+ ' (�������� ������)';
     255 : Msg := Msg+ ' (������� ���������� �������������)';
 end;
 AddLogLine(2, 'A', Msg);
end;

procedure TfrmMain.miShowHideClick(Sender: TObject);
begin
 frmMain.Visible := not frmMain.Visible;
 if frmMain.Visible then miShowHide.Caption := '������ ���'
   else miShowHide.Caption := '�������� ���';
end;

procedure TfrmMain.TrayIconStartup(Sender: TObject;
  var ShowMainForm: Boolean);
begin
 ShowMainForm := false;
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

procedure TfrmMain.hcLogSectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
 redtLog.Tab[1] := hcLog.Sections[0].Width + hcLog.Sections[1].Width;
 HideCaret(redtLog.Handle);
end;

end.
