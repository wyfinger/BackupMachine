unit LogEdit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, ComCtrls, Messages, Windows, RichEdit,
    Graphics;

type
  TLinkClicked = procedure (Sender: TObject;
    cpMin, cpMax: LongInt;
    const lpstrText: string
  ) of object;

  TLogEdit = class(TRichEdit)
  private
    { Private declarations }
    FLinkClicked: TLinkClicked;
    procedure SetHyperlink(Hyperlink: Boolean; wParam: Integer);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
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
    procedure CreateWnd; override;
  public
    { Public declarations }
    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
    procedure LinkSelection(Enable: Boolean);
  published
    { Published declarations }
    property OnHyperlinkClicked: TLinkClicked read FLinkClicked Write FLinkClicked;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TLogEdit]);
end;

{ TLogEdit }

procedure TLogEdit.CreateWnd;
begin
  inherited;
  // Updating Richedit EventMask.
  // Adding hyperlinks support
  SendMessage (
    Handle,
    EM_SETEVENTMASK, 0,
    SendMessage(Handle, EM_GETEVENTMASK, 0,0) or ENM_LINK
  );
end;

procedure TLogEdit.CNNotify(var Message: TWMNotify);
type
  PENLink = ^TENLink;
var
  TR: TextRange;
begin
  if (Message.NMHdr^.code=EN_LINK) then
  begin
    if (PENLink(Message.NMHdr).Msg=WM_LBUTTONDOWN) and Assigned(FLinkClicked) then
    begin
      TR.chrg := PENLink(Message.NMHdr).chrg;
      GetMem(TR.lpstrText, TR.chrg.cpMax - TR.chrg.cpMin + 2);
      try
        SendMessage(Handle, EM_GETTEXTRANGE, 0, Integer(@TR));
        FLinkClicked(Self, TR.chrg.cpMin, TR.chrg.cpMax, TR.lpstrText);
      finally
        FreeMem(TR.lpstrText);
      end;
    end;
    Message.Result := 0;
  end
  else
    inherited;
end;

procedure TLogEdit.SetHyperlink(Hyperlink: Boolean; wParam: Integer);
var
  cf: TCharFormat;
begin
  FillChar(cf, SizeOf(cf), 0);
  cf.cbSize := SizeOf(cf);

  cf.dwMask := CFM_LINK or CFM_COLOR or CFM_UNDERLINE;
  if Hyperlink then
  begin
    cf.dwEffects := CFE_LINK or CFE_UNDERLINE;
    cf.crTextColor := COLORREF(clBlue);
  end
  else
    cf.crTextColor := Font.Color;
  SendMessage(Handle, EM_SETCHARFORMAT, wParam, integer(@cf));
end;

procedure TLogEdit.LinkSelection(Enable: Boolean);
begin
 SetHyperlink(Enable, SCF_SELECTION);
end;                                 

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
 //HideCaret(Handle);
 //DestroyCaret;
end;

procedure TLogEdit.WMLButtonDown(var Message: TWMLButtonDown);
begin
 inherited;
 //HideCaret(Handle);
 //DestroyCaret;
end;

procedure TLogEdit.WMMButtonDown(var Message: TWMMButtonDown);
begin
 inherited;
 //HideCaret(Handle);
 //DestroyCaret;
end;

procedure TLogEdit.WMRButtonDown(var Message: TWMRButtonDown);
begin
 inherited;
 //HideCaret(Handle);
 //DestroyCaret;
end;

procedure TLogEdit.WMSetCursor(var Message: TWMSetCursor);
begin
 inherited;
 //HideCaret(Handle);
 //DestroyCaret;
end;

procedure TLogEdit.WMSetFocus(var Message: TWMSetFocus);
begin
 inherited;
 //HideCaret(Handle);
 //DestroyCaret;
end;




end.
