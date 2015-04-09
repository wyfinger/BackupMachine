unit LogEdit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, ComCtrls, Messages, Windows;

type
  TLogEdit = class(TRichEdit)
  private
    { Private declarations }
    function GetTab(Index: Byte): Longint;
    procedure SetTab(Index: Byte; Value: Longint);
    procedure SetTabCount(Value: Integer);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    function GetTabCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
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

function TLogEdit.GetTab(Index: Byte): Longint;
begin
 Result := Paragraph.Tab[Index] * GetDeviceCaps(GetDC(Handle), LOGPIXELSX) div 72;
end;

function TLogEdit.GetTabCount: Integer;
begin
 Result := Paragraph.TabCount;
end;

procedure TLogEdit.SetTab(Index: Byte; Value: Integer);
var
  tSelStart, tSelLen : Integer;
begin
 SendMessage(Handle, WM_SETREDRAW, Integer(False), 0);
 tSelStart := SelStart;
 tSelLen := SelLength;
 SelectAll;
 Paragraph.Tab[Index] := Value * 72 div GetDeviceCaps(GetDC(Handle), LOGPIXELSX);
 SelStart := tSelStart;
 SelLength := tSelLen;
 SendMessage(Handle, WM_SETREDRAW, Integer(True), 0);
 Repaint;
 HideCaret(Handle);
 DestroyCaret;
end;

procedure TLogEdit.SetTabCount(Value: Integer);
begin
 Paragraph.TabCount := Value;
end;

procedure TLogEdit.WMKeyDown(var Message: TWMKeyDown);
begin
 inherited;
 HideCaret(Handle);
 DestroyCaret;
end;

procedure TLogEdit.WMLButtonDown(var Message: TWMLButtonDown);
begin
 inherited;
 HideCaret(Handle);
 DestroyCaret;
end;

procedure TLogEdit.WMMButtonDown(var Message: TWMMButtonDown);
begin
 inherited;
 HideCaret(Handle);
 DestroyCaret;
end;

procedure TLogEdit.WMRButtonDown(var Message: TWMRButtonDown);
begin
 inherited;
 HideCaret(Handle);
 DestroyCaret;
end;

procedure TLogEdit.WMSetCursor(var Message: TWMSetCursor);
begin
 inherited;
 HideCaret(Handle);
 DestroyCaret;
end;

procedure TLogEdit.WMSetFocus(var Message: TWMSetFocus);
begin
 inherited;
 HideCaret(Handle);
 DestroyCaret;
end;



end.
