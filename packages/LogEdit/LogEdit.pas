unit LogEdit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, ComCtrls, Messages, Windows;

type
  TLogEdit = class(TRichEdit)
  private
    { Private declarations }
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TLogEdit]);
end;

{ TLogEdit }

procedure TLogEdit.WMSetFocus(var Message: TWMSetFocus);
begin
 //Message.Result := 0;
 if Message.Msg = WM_SETFOCUS then
   begin
     //Message.Result := 1;
     HideCaret(Self.Handle);
   end;
end;

end.
