program BMachine;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Backup Machine';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
