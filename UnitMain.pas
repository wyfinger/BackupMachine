unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DirMon, IniFiles, Registry, process, CoolTrayIcon,
  Menus, ShellAPI, ImgList, ComCtrls, RichEdit, LogEdit, WinInet, HTTPGet, StrUtils,
  pngimage;

type
  TfrmMain = class(TForm)
    tmrArchive: TTimer;
    DirMon: TDirMon;
    Process: TProcess;
    TrayIcon: TCoolTrayIcon;
    pmTray: TPopupMenu;
    miShowHide: TMenuItem;
    miSep2: TMenuItem;
    miExit: TMenuItem;
    miConfig: TMenuItem;
    ilError: TImageList;
    ilArchive: TImageList;
    hcLog: THeaderControl;
    btn1: TButton;
    redtLog: TLogEdit;
    ilProgress: TImageList;
    tmrProgress: TTimer;
    miSep1: TMenuItem;
    miOpenFF: TMenuItem;
    miOpenBF: TMenuItem;
    tmrConfig: TTimer;
    httpGet: THTTPGet;
    imgSource: TImage;
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
    procedure tmrProgressTimer(Sender: TObject);
    procedure miOpenFFClick(Sender: TObject);
    procedure miOpenBFClick(Sender: TObject);
    procedure tmrConfigTimer(Sender: TObject);
    procedure httpGetDoneString(Sender: TObject; Result: String);
    procedure httpGetError(Sender: TObject);
    procedure redtLogHyperlinkClicked(Sender: TObject; cpMin,
      cpMax: Integer; const lpstrText: String);
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
    procedure AnimateProgress;
    procedure ReloadConfig();
    procedure GetProxyData(var ProxyEnabled: boolean; var ProxyServer: string; var ProxyPort: integer);
    procedure PrepareIcons(bgColor: TColor);
  public
    { Public declarations }
    FConfigFile     : string;
    FFilesFolder    : string;   // what
    FArchivesFolder : string;   // where
    FRarPath        : string;   // RAR path
    FCommand        : string;   // params
    FPeriod         : Integer;  // min period
    FAppMark        : string;
    FAppColor       : TColor;
    FAutostart      : Boolean;
    FShowRar        : Boolean;
    FLogLevel       : Integer;
    FBaloonTime     : Integer;

    nxLogPixels     : Integer;
    nTextFontWidth  : Integer;

    FConfigFileDate : Integer;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 redtLog.TabCount := 2;
 redtLog.Tab[0] := hcLog.Sections[0].Width;
 redtLog.Tab[1] := hcLog.Sections[0].Width + hcLog.Sections[1].Width;

 FFilesList := TStringList.Create;

 Left := Screen.WorkAreaRect.Right - Width;
 Top := Screen.WorkAreaRect.Bottom - Height;    

 Caption := Caption + ' ' + GetSelfVersion();
 TrayIcon.Hint := 'Backup Machine ' + GetSelfVersion();

 ReloadConfig();
end;

// Load configuration file
procedure TfrmMain.ReloadConfig;
var
  tAutostart,
  tShowRar,
  tTopMost,
  tCheckUpd : Integer;
  tFHandle  : THandle;
begin
 FConfigFile := ExtractFilePath(Application.ExeName) + 'BMachine.ini';
 FSettings := TIniFile.Create(FConfigFile);
 FFilesFolder    := FSettings.ReadString('path', 'files_folder', '');
 FArchivesFolder := FSettings.ReadString('path', 'archives_folder', '');
 FAppMark        := Trim(FSettings.ReadString('system', 'app_mark', ''));
 if FAppMark <> '' then
   begin
     TrayIcon.Hint := 'Backup Machine ' + GetSelfVersion() + ' - ' + FAppMark;
     Caption := 'Backup Machine Log ' + GetSelfVersion() + ' - ' + FAppMark;
   end;
 try
   FAppColor := StrToInt('$'+Trim(UpperCase(FSettings.ReadString('system', 'app_color', '009900'))));
   // BGR -> RGB :)
   FAppColor := FAppColor or (FAppColor and $000000FF) shl 24; // X := b
   FAppColor := FAppColor and $FFFFFF00;                       // b := a
   FAppColor := FAppColor or (FAppColor and $00FF0000) shr 16;
   FAppColor := FAppColor and $FF00FFFF;                       // a := X
   FAppColor := FAppColor or (FAppColor and $FF000000) shr 8;
   FAppColor := FAppColor and $00FFFFFF;    
 except
   FAppColor := $009900;
 end;
 FRarPath        := FSettings.ReadString('system', 'rar_path', '');
 FCommand        := FSettings.ReadString('system', 'command', '');
 FCommand        := StringReplace(FCommand, '%FF%', FFilesFolder, [rfReplaceAll]);
 FCommand        := StringReplace(FCommand, '%AF%', FArchivesFolder, [rfReplaceAll]);
 FPeriod         := FSettings.ReadInteger('system', 'period', 5)*1000*60;  // минуты
 tAutostart      := FSettings.ReadInteger('system', 'autostart', 1);
 FAutostart      := tAutostart <> 0;
 tShowRar        := FSettings.ReadInteger('system', 'show_rar_mode', 0);
 FShowRar        := tShowRar <> 0;
 tTopMost        := FSettings.ReadInteger('system', 'topmost', 0);
 if tTopMost <> 0 then FormStyle := fsStayOnTop;
 tCheckUpd       := FSettings.ReadInteger('system', 'check_updates', 0);
 if tCheckUpd <> 0 then httpGet.GetString;
 FLogLevel       := FSettings.ReadInteger('system', 'loglevel', 7);
 FBaloonTime     := FSettings.ReadInteger('system', 'ballon_time', 10);
 FSettings.Free;

 if FBaloonTime < 10 then FBaloonTime := 10;
 if FBaloonTime > 60 then FBaloonTime := 60;

 if FShowRar then Process.ShowWindow := swShowNormal
   else Process.ShowWindow := swHide;

 CheckAutostart(FAutostart);

  if not FileExists(FRarPath) then
   AddLogLine(0, 'E', 'Can''t find RAR executable file, monitor is not started');
 if not DirectoryExists(FFilesFolder) then
   AddLogLine(0, 'E', 'Files folder is not found, monitor is not started');
 if not DirectoryExistsEx(FArchivesFolder) then
   AddLogLine(0, 'E', 'Dest folder for archives is not found, monitor is not started');

 PrepareIcons(FAppColor);

 // «апомним дату изменени€ файла настроек
 tFHandle := FileOpen(FConfigFile, fmOpenRead  or fmShareDenyNone);
 if tFHandle <> INVALID_HANDLE_VALUE then
   begin
     FConfigFileDate := FileGetDate(tFHandle);
     CloseHandle(tFHandle);
   end;

 if FileExists(FRarPath) and
    DirectoryExists(FFilesFolder) and
    DirectoryExistsEx(FArchivesFolder) then
      begin
        tmrArchive.Interval := FPeriod;
        tmrArchive.Enabled := True;
        DirMon.Path := FFilesFolder;
        DirMon.Active := True;
        AddLogLine(2, 'I', 'Load settings and start monitor');
        TrayIcon.IconList := ilProgress;
        TrayIcon.CycleIcons := False;
      end else
      begin
        TrayIcon.IconList := ilError;
        TrayIcon.CycleIcons := True;
      end;
end;

procedure TfrmMain.httpGetDoneString(Sender: TObject; Result: String);
var
  v, m           : string;
  v1, v2, v3, v4 : Integer;
  m1, m2, m3, m4 : Integer;
  p1, p2         : Integer;
  link, msg      : string;
  spos           : Integer;
begin
 if Length(Result) = 0 then Exit;
 try
   p1 := Pos(':', Result);
   p2 := Pos(#13, Result);
   if p2 = 0 then p2 := Pos(#10, Result);
   link := Copy(Result, p1+1, p2-p1-1);
   // верси€ на сервере
   v := Copy(Result, 1, p1-1);
   // наша верси€
   m := GetSelfVersion();
   v1 := Pos('.', v);
   v2 := PosEx('.', v, v1+1);
   v3 := PosEx('.', v, v2+1);
   v4 := StrToInt(Copy(v, v3+1, 3));
   v3 := StrToInt(Copy(v, v2+1, v3-v2-1));
   v2 := StrToInt(Copy(v, v1+1, v2-v1-1));
   v1 := StrToInt(Copy(v, 0, v1-1));
   v1 := v4 + v3*1000 + v2 * 100000 + v1 * 10000000;
   m1 := Pos('.', m);
   m2 := PosEx('.', m, m1+1);
   m3 := PosEx('.', m, m2+1);
   m4 := StrToInt(Copy(m, m3+1, 3));
   m3 := StrToInt(Copy(m, m2+1, m3-m2-1));
   m2 := StrToInt(Copy(m, m1+1, m2-m1-1));
   m1 := StrToInt(Copy(m, 0, m1-1));
   m1 := m4 + m3*1000 + m2 * 100000 + m1 * 10000000;
   if (v1 > m1) then
     begin
       AddLogLine(2, 'U', 'New version '+ v +' is avaible: ');
       // hack - delete CrLr at the end
       spos := Length(redtLog.Text)-2;  // #13#10
       redtLog.Lines[redtLog.Lines.Count-1] := redtLog.Lines[redtLog.Lines.Count-1] + link;
       redtLog.SelStart := spos;
       redtLog.SelLength := Length(link);
       redtLog.LinkSelection(true);
       redtLog.SelLength := 0;
     end;
 except
   AddLogLine(0, 'E', 'Update check error, response is received, but it is corrupted');
 end;
end;

procedure TfrmMain.httpGetError(Sender: TObject);
begin
 AddLogLine(0, 'E', 'Can''t check updates');
end;

procedure TfrmMain.CheckAutostart(Autostart: Boolean);
begin
 with TRegistry.Create do
   try
     RootKey := HKEY_CURRENT_USER;
     if OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Run', true) then
       if Autostart then WriteString('BackupMachine_'+FAppMark, Application.ExeName)
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

// check folder exists
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

// add string line to the log
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
     if Level = 0 then  // errors
       begin
         Color := clRed;
         Style := [];
       end;
     if Level = 1 then  // monitoring
       begin
         Color := clWindowText;
         Style := [];
       end;
     if Level = 2 then  // srart RAR process
       begin
         Color := clWindowText;
         Style := [fsBold];
       end;
   end;
 redtLog.Lines.Add('  '+ Tag +#9+ DateStr +#9+ Description);
 redtLog.ScrollBy(0, 999);
 // popups message
 if ((FLogLevel and 4) = 4) and (Level = 0) then
   TrayIcon.ShowBalloonHint('Error', Description, bitError, FBaloonTime);
 if ((FLogLevel and 8) = 8) and (Level = 0) then
   TrayIcon.ShowBalloonHint('Archive', Description, bitInfo, FBaloonTime);
end;

// icon animation if files changed
procedure TfrmMain.AnimateProgress;
begin
 if TrayIcon.IconList = ilError then Exit;
 TrayIcon.IconList := ilProgress;
 TrayIcon.CycleIcons := True;
 tmrProgress.Enabled := False; // reset timer
 tmrProgress.Enabled := True;
end;

procedure TfrmMain.DirMonCreated(Sender: TObject; FileName: String);
begin
 if (FileName <> FLastFile) and not DirectoryExists(FFilesFolder+FileName) and
   FileExists(FFilesFolder+FileName) then
   begin
     AddLogLine(1, 'C', FFilesFolder+FileName);
     FFilesList.Add(FFilesFolder+FileName);
     FLastFile := FileName;
     AnimateProgress();
   end;
end;

procedure TfrmMain.DirMonDeleted(Sender: TObject; FileName: String);
begin
 if not DirectoryExists(FileName) then
   begin
     AddLogLine(1, 'D', FFilesFolder+FileName);
     AnimateProgress();
   end;
end;

procedure TfrmMain.DirMonModified(Sender: TObject; FileName: String);
begin
 if (FileName <> FLastFile) and not DirectoryExists(FFilesFolder+FileName) and
   FileExists(FFilesFolder+FileName) then
   begin
     AddLogLine(1, 'M', FFilesFolder+FileName);
     FFilesList.Add(FFilesFolder+FileName);
     FLastFile := FileName;
     AnimateProgress();
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
     AnimateProgress();
   end;
end;

procedure TfrmMain.GetProxyData(var ProxyEnabled: boolean; var ProxyServer: string; var ProxyPort: integer);
var
  ProxyInfo: PInternetProxyInfo;
  Len: LongWord;
  i, j: integer;
begin
  Len := 4096;
  ProxyEnabled := false;
  GetMem(ProxyInfo, Len);
  try
    if InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len)
    then
      if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
      begin
        ProxyEnabled:= True;
        ProxyServer := ProxyInfo^.lpszProxy;
        showmessage('!');
      end
  finally
    FreeMem(ProxyInfo);
  end;
 
  if ProxyEnabled and (ProxyServer <> '') then
  begin
    i := Pos('http=', ProxyServer);
    if (i > 0) then
    begin
      Delete(ProxyServer, 1, i+5);
      j := Pos(';', ProxyServer);
      if (j > 0) then
        ProxyServer := Copy(ProxyServer, 1, j-1);
    end;
    i := Pos(':', ProxyServer);
    if (i > 0) then
    begin
      ProxyPort := StrToIntDef(Copy(ProxyServer, i+1, Length(ProxyServer)-i), 0);
      ProxyServer := Copy(ProxyServer, 1, i-1)
    end
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
       TrayIcon.IconList := ilError;
       TrayIcon.CycleIcons := True;
       Exit;
     end;
     TrayIcon.IconList := ilArchive;
     TrayIcon.CycleIcons := True;
     FFilesList.Clear;
     Process.AppName := FRarPath;
     Process.CommandLine := FCommand;
     Process.Directory := FArchivesFolder;
     Process.Execute;
     FLastFile := '';
   end;
 // iss#12
 // if archive process is active than twice the timer interval
 if Process.Active then
   begin
     AddLogLine(0, 'D', 'Archive process is active, change archive interval to ' +
       IntToStr(Trunc(2 * tmrArchive.Interval / 60000)) + ' min');
     tmrArchive.Interval := 2 * tmrArchive.Interval;
   end;
end;

procedure TfrmMain.ProcessFinished(Sender: TObject; ExitCode: Cardinal);
var
  DateStr : string;
  Msg     : string;
begin
 DateTimeToString(DateStr, 'hh:mm:ss', Now());
 TrayIcon.IconList := ilProgress;
 TrayIcon.CycleIcons := False;
 TrayIcon.IconIndex := 0;
 Msg := 'RAR process is ended';
 case ExitCode of
       1 : Msg := Msg+ ' (Non fatal error(s) occurred)';
       2 : Msg := Msg+ ' (A fatal error occurred)';
       3 : Msg := Msg+ ' (Invalid checksum. Data is damaged)';
       4 : Msg := Msg+ ' (Attempt to modify an archive locked by ''k'' command)';
       5 : Msg := Msg+ ' (Write error)';
       6 : Msg := Msg+ ' (File open error)';
       7 : Msg := Msg+ ' (Wrong command line option)';
       8 : Msg := Msg+ ' (Not enough memory)';
       9 : Msg := Msg+ ' (File create error)';
      10 : Msg := Msg+ ' (No files matching the specified mask and options were found)';
      11 : Msg := Msg+ ' (Wrong password)';
     255 : Msg := Msg+ ' (User stopped the process)';
 end;
 AddLogLine(2, 'A', Msg);
end;

procedure TfrmMain.miShowHideClick(Sender: TObject);
begin
 frmMain.Visible := not frmMain.Visible;
 if frmMain.Visible then miShowHide.Caption := 'Hide log'
   else miShowHide.Caption := 'Show log';
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

procedure TfrmMain.tmrProgressTimer(Sender: TObject);
begin
 TrayIcon.CycleIcons := False;
 TrayIcon.IconIndex := 0;
 tmrProgress.Enabled := False;
end;

procedure TfrmMain.miOpenFFClick(Sender: TObject);
begin
 ShellExecute(handle, 'open', PChar(FFilesFolder), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.miOpenBFClick(Sender: TObject);
begin
 ShellExecute(handle, 'open', PChar(FArchivesFolder), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.tmrConfigTimer(Sender: TObject);
var
  tFHandle : THandle;
begin
 // reload config if it changed
 tFHandle := FileOpen(FConfigFile, fmOpenRead  or fmShareDenyNone);
 if tFHandle <> INVALID_HANDLE_VALUE then
   begin
     if FileGetDate(tFHandle) > FConfigFileDate then ReloadConfig();
     CloseHandle(tFHandle);
   end;
end;

procedure TfrmMain.redtLogHyperlinkClicked(Sender: TObject; cpMin,
  cpMax: Integer; const lpstrText: String);
begin
  ShellExecute(handle, 'open', PChar(lpstrText), nil, nil, SW_SHOWNORMAL);
end;


procedure TfrmMain.PrepareIcons(bgColor: TColor);
var
  ico : TBitmap;
  r , c, i : Integer;
begin
 // prepare icons with bitmap
 i := 0;
 for r := 0 to imgSource.Width div 16 -1 do
   for c := 0 to imgSource.Height div 16 -1 do
     begin
       ico := TBitmap.Create;
       ico.Width := 16; ico.Height := 16;
       ico.Canvas.Brush.Color := bgColor;
       ico.Canvas.Pen.Color := bgColor;
       ico.Canvas.FillRect(Rect(0,0,16,16));
       // paste icon from image source
       ico.Canvas.Draw(-16*c, -16*r, imgSource.Picture.Graphic);
       // set to image lists
       case i of
         1..10 : ilArchive.Add(ico, nil);
         11..12 : ilProgress.Add(ico, nil);
         0,13 : begin                    // 2 icons
                  ilError.Add(ico, nil);
                  ilError.Add(ico, nil);
                end;
       end;
       //
       ico.Free;
       //
       i := i+1;
     end;
 // update window icon
 ilProgress.GetIcon(0, Icon);
 ilProgress.GetIcon(0, Application.Icon);
 TrayIcon.IconList := ilProgress;
end;

end.
