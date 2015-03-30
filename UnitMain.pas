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
  protected
    { Protected declarations }
    FSettings  : TIniFile;
    FFilesList : TStringList;
  private
    { Private declarations }
    procedure CheckAutostart(Autostart: Boolean);
  public
    { Public declarations }
    FFilesFolder    : string;   // Что архивировать
    FArchivesFolder : string;   // Куда архивировать
    FRarPath        : string;   // Путь к RAR
    FCommand        : string;   // Параметры
    FPeriod         : Integer;  // Минимальная периодичность
    FAutostart      : Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  tAutostart : Integer;
begin
 // Загрузка параметров из конфигурационного файла
 FSettings := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'BMachine.ini');
 FFilesFolder    := FSettings.ReadString('path', 'files_folder', '');
 FArchivesFolder := FSettings.ReadString('path', 'archives_folder', '');
 FRarPath        := FSettings.ReadString('system', 'rar_path', '');
 FCommand        := FSettings.ReadString('system', 'command', '');
 FPeriod         := FSettings.ReadInteger('system', 'period', 5)*1000*60;  // минуты
 tAutostart      := FSettings.ReadInteger('system', 'autostart', 1);
 FAutostart      := tAutostart <> 0;
 CheckAutostart(FAutostart);

 if FileExists(FRarPath) and
    DirectoryExists(FFilesFolder) {and
    DirectoryExists(FArchivesFolder)} then
      begin
        tmrArchive.Interval := FPeriod;
        tmrArchive.Enabled := True;
        DirMon.Path := FFilesFolder;
        DirMon.Active := True;
      end;
 FFilesList := TStringList.Create;
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

procedure TfrmMain.DirMonCreated(Sender: TObject; FileName: String);
begin
 mmoLog.Lines.Add('Создание файла: "'+FileName+'"');
 FFilesList.Add(FFilesFolder+FileName);
end;

procedure TfrmMain.DirMonDeleted(Sender: TObject; FileName: String);
begin
 mmoLog.Lines.Add('Удаление файла: "'+FileName+'"');
end;

procedure TfrmMain.DirMonModified(Sender: TObject; FileName: String);
begin
 mmoLog.Lines.Add('Изменение файла: "'+FileName+'"');
 FFilesList.Add(FFilesFolder+FileName);
end;

procedure TfrmMain.DirMonRenamed(Sender: TObject; fromFileName,
  toFileName: String);
begin
  mmoLog.Lines.Add('Переименование файла: "'+fromFileName+'" в "'+toFileName+'"');
  FFilesList.Add(FFilesFolder+toFileName);
end;

procedure TfrmMain.mmoLogChange(Sender: TObject);
begin
 mmoLog.ScrollBy(0, 1000);
end;

procedure TfrmMain.tmrArchiveTimer(Sender: TObject);
var
  DateStr: string;
begin
 if (FFilesList.Count > 0) and not Process.Active then
   begin
     DateTimeToString(DateStr, 'hh:mm:ss', Now());
     mmoLog.Lines.Add(DateStr + ' Начало архивирования....');
     FFilesList.SaveToFile(FArchivesFolder+'files.lst');
     FFilesList.Clear;
     Process.AppName := FRarPath;
     Process.CommandLine := FCommand;
     Process.Directory := FArchivesFolder;
     Process.Execute;
   end;
end;

procedure TfrmMain.ProcessFinished(Sender: TObject; ExitCode: Cardinal);
var
  DateStr: string;
begin
 DateTimeToString(DateStr, 'hh:mm:ss', Now());
 mmoLog.Lines.Add(DateStr + ' .... Окончание архивирования');
end;

procedure TfrmMain.miShowHideClick(Sender: TObject);
begin
 frmMain.Visible := not frmMain.Visible;
 if frmMain.Visible then miShowHide.Caption := 'Скрыть лог'
   else miShowHide.Caption := 'Показать лог';
end;

procedure TfrmMain.mmoLogMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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
 Close;
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
end;

end.
