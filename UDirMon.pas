unit UDirMon;

interface

uses
  SysUtils, Windows, Classes;

type
  {��� ������ � ������� �� ��������� ����� (��� �����).
  http://msdn.microsoft.com/en-us/library/windows/desktop/aa364391%28v=vs.85%29.aspx}
  FILE_NOTIFY_INFORMATION = packed record
    //�������� (� ������) �� ��������� ������ ���� FILE_NOTIFY_INFORMATION.
    NextEntryOffset,
    //��� �������� (���������).
    Action,
    //������ ������� � ������, � ������� �������� ��� �����.
    FileNameLength : Integer;
    //������ ������ � ����� �����. ��� ����� ������������ �������� ����������� ��������.
    FileName : WideChar;
  end;
  TPFni = ^FILE_NOTIFY_INFORMATION;

  TDmThr = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    //���� � �����, ������� ����� �����������.
    FPath : String;
    //��������� ����, ������� ��������� �������� � ����������� ����� ������ � ����� FBuff.
    FHNotify : THandle;
    //����� ���������, ������� ������ ���������� ���� FHNotify.
    FWMNotify : Cardinal;
    //������� ��� ���������������� ������� ������� � ����������� ������ FBuff.
    FHMtxBuff : THandle;
    {���������� �����. � ���� ����� ������������ �������� �� �������������
    ���������� � ������ � ������. ������ ����� ������ ����� ������������
    �������� �����.}
    FBuff : array of Byte;
    //������ �������� ������ � ���������� ������ (� ������).
    FBuffSize : Cardinal;
    //��� ������, ������� ����� ��������� ��� ������ API �������.
    FErr : Integer;
  end;

implementation

{ TThr }

procedure TDmThr.Execute;
const
  {������ ������� � ������. ��� ������ ���� ������, � ������� ������� �����
  ���������� �������� �� ������������� ����������.
  ���������� ���������� ������ ������ ������� = High(Word) = 65536 ����.}
  SizeBuff = High(Word);
var
  hDir, hEv : THandle;
  Buff : array of Byte;
  SizeRet, ResWait : Cardinal;
  Ovlp : _OVERLAPPED;
  PFni : TPFni;
begin
  //��������.
  if FHNotify = 0 then
    raise Exception.Create('�������. ��������� ���������� ��������� �� �����!');
  if FWMNotify = 0 then
    raise Exception.Create('�������. ����� ��������� �� �����!');
  if FHMtxBuff = 0 then
    raise Exception.Create('�������. ������ �� ��������!');
  //�������������.
  hEv := 0;
  //�������� ��������� �����.
  hDir := CreateFile(PChar(FPath), GENERIC_READ, FILE_SHARE_READ
    or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
  //�������� ���������.
  if hDir = INVALID_HANDLE_VALUE then begin
    FErr := GetLastError;
    Exit;
  end;
  try
    {�������� ������ ��� ������. � ���� ����� ������� ����� ����������
    �������� �� ������������� ���������� � ������ � ������.}
    SetLength(Buff, SizeBuff);
    {������ ���������� ������ (������-�������) � �������������� �������,
    � ������������� (�������) ��������� � �������� ��� ���������.}
    hEv := CreateEvent(nil, False, False, nil);
    if hEv = 0 then begin
      FErr := GetLastError;
      Exit; //��� ���� ���������� ������� � ������ finally - end.
    end;
    //���������� ��������� �������-������� � ��������� Ovlp.
    Ovlp.hEvent := hEv;
    //����������.
    while not Terminated do begin
      {���������� �� ������.
      ��� ������ ������ ReadDirectoryChangesW() ������� ������ ����� � ������,
      ��������� ��� � �������� ���������� hDir � ��������� ����������. � ����
      ����� ������� ���������� �������� �� ���������� ������ � �����. �������
      ����� ���������� ���������� �� �������, ����� ����� ��������� ���������
      ����� - ����� ����� CloseHandle(hDir).
      ��� ������ ������ ReadDirectoryChangesW() (� ��� ������ ������ - ����)
      ������� ������ �� ��������� �������� �� ���������� � �� ������ �� �
      ����� Buff.
      ���� ��������� ������������� ��������� � ������� �������� �������� � ���
      � ����� Buff, �� ������� ���������� � ��������� Ovlp �������� � ����������
      ������ � ������������� ������-������� Ovlp.hEvent � ���������� ���������.}
      if not ReadDirectoryChangesW(hDir, Buff, SizeBuff, True,
        FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_DIR_NAME
        or FILE_NOTIFY_CHANGE_ATTRIBUTES or FILE_NOTIFY_CHANGE_SIZE
        or FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_LAST_ACCESS
        or FILE_NOTIFY_CHANGE_CREATION or FILE_NOTIFY_CHANGE_SECURITY,
        nil, @Ovlp, nil)
      then begin
        FErr := GetLastError;
        Exit;
      end;
      {����� ���������� ���������. �� ������ �������� ����� ����������� �����
      WaitForSingleObject(). ��� ������� 500 ����������� ��� �������, �����
      ������-������� Ovlp.hEvent ������� � ���������� ���������. ���� �� ���
      ����� ������-������� �� ������� � ���������� ���������, �� �������
      WaitForSingleObject() �������� ���� ������ � ��������� ��������: WAIT_TIMEOUT.
      ���� WaitForSingleObject() ���������, ��� ������-������� ��������
      � ���������� ���������, �� WaitForSingleObject() �������� ���� ������
      � ��������� ��������: WAIT_OBJECT_0. � ���� ������ ������� �������������
      �������� ������-������� � ������������ ��������� (��������� ���������).
      � ����� ������ �������� ����� ����������� �������� ����� ���������� ������
      Terminated � ��������� ������ ������� WaitForSingleObject().
      ����� �� ����� ����������, ���� Terminated = True - �. �., ���������� ����
      ���������� ������. ��� ���� ResWait <> WAIT_TIMEOUT - �. �., �������
      WaitForSingleObject() ����������, ��� ������-������� ������� � ����������
      ��������� ���, ���� ��������� ������.}
      repeat
        ResWait := WaitForSingleObject(hEv, 500);
      until Terminated or (ResWait <> WAIT_TIMEOUT);

      {���� ������-������� ������� � ���������� ���������. � ���� ������ ��� ����
      ������� ������ �� ������ Buff.}
      if ResWait = WAIT_OBJECT_0 then begin
        {��������� ������ ������, ������� �������� � ����� Buff. ��� �����
        �������� GetOverlappedResult(). ������ ����� ������� � ���������� SizeRet.
        ����� ��������� �������� ������� ����� ��������. True - ������� ��
        �������, ����� �������� �������� ������ ����������. False - �� �������.
        � ����� ������ ��������, ��� �������� ��� �����������, ������� ��������
        �������� ������������� � False.}
        if not GetOverlappedResult(hDir, Ovlp, SizeRet, False) then begin
          FErr := GetLastError;
          Exit;
        end;
        {�������� ������ �� ������ Buff � ���������� �� � ���������� �����.
        ��� ���� ���������� �����, ��� ������� ������ � ����������� ������
        �������� ������ ���� ����� - �������� ��� ��������������. ���� �����
        ������������ ������ �������, ������ ����� ������ �������. �����
        ������ ����� �������� ������ � �������, ����� ������ � ������ �����
        �������� ������ �����. � ������ ��� ������ �����, ���� �������� ����������
        � ������, ������ ����� �������, ���� ����� ������������ ������ �������.
        ��� ����������� ������ ������ �������� ������� (MUTEX). ������� ������
        � �������� ������ � ��� ��������� ������� � ���� TThr.FHMtxBuff.
        ��� �������, ����� ������� ������ ��������� � ����������� ���.}
        if WaitForSingleObject(FHMtxBuff, INFINITE) = WAIT_OBJECT_0 then begin
          //���� ���������, ����������� ������ ����������� ������.
          if Length(FBuff) < Integer(FBuffSize + SizeRet) then
            SetLength(FBuff, FBuffSize + SizeRet);
          //��������� � ���������� ����� ����� ������.
          CopyMemory(@FBuff[FBuffSize], @Buff[0], SizeRet);
          {���� ����� ����������� ����� ������ ����� ��� ��������, �� � ���������
          ������ ������� ������ ���������� �������� � ��������.}
          if FBuffSize > 0 then begin
            //��������� �� ������ ������ � ������.
            PFni := @FBuff[0];
            //���� ��������� �� ��������� ������ � ������� ������.
            while PFni^.NextEntryOffset <> 0 do
              Inc(PByte(PFni), PFni^.NextEntryOffset);
            //� ��������� ������ ���������� �������� � ��������.
            PFni^.NextEntryOffset := Integer(@FBuff[FBuffSize]) - Integer(PFni);
          end;
          {������ �������� ������ � ���������� ������ ������ ����������
          �� SizeRet ����.}
          Inc(FBuffSize, SizeRet);
          //��������� �������.
          ReleaseMutex(FHMtxBuff);
          {�������� ���������� ���������, ����������, ��� � ���������� �����
          ��������� ����� ������.}
          PostMessage(FHNotify, FWMNotify, 0, 0);
        end;
      //���� ��������� ������.
      end else begin
        FErr := GetLastError;
        Exit;
      end;
    end;
  finally
    CloseHandle(hEv);
    CloseHandle(hDir);
  end;
end;

end.
