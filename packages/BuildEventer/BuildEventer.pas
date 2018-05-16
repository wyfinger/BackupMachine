//
// BuildEventer for Delphi
// Install this package and Delphi will start BeforeBuild.bat file on
// project folder before build.
// I use it for compile project sources into EXE file.
//
// Wyfinger, 2017-03-10
// https://github.com/wyfinger
//

unit BuildEventer;

interface

uses
  ToolsAPI, ShellAPI, Windows;

type
  TBuildEventer = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    procedure FindAndStartBatFile(ProjectFile, BatFileName: string);
  protected
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel:
      boolean);
    procedure  FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
     var Cancel: Boolean);
  end;

procedure Register;

var
  FNotifierIndex: Integer;

implementation

uses SysUtils;

procedure Register;
begin
 FNotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(TBuildEventer.Create);
end;

procedure TBuildEventer.FindAndStartBatFile(ProjectFile, BatFileName: string);
var
  CommandFile: string;
begin
 if Trim(ProjectFile) = '' then Exit;
 CommandFile := IncludeTrailingBackslash(ExtractFileDir(ProjectFile)) + BatFileName;
 if FileExists(CommandFile) then
   ShellExecute(0, nil, PChar(CommandFile), nil, nil, SW_HIDE);
end;

procedure TBuildEventer.AfterCompile(Succeeded: Boolean);
begin
 //
end;

procedure TBuildEventer.BeforeCompile(const Project: IOTAProject;
  var Cancel: boolean);
begin
 FindAndStartBatFile(Project.FileName, 'BeforeBuild.bat');
end;

procedure TBuildEventer.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
 //
end;

initialization
finalization
  (BorlandIDEServices as IOTAServices).RemoveNotifier(FNotifierIndex);
end.
