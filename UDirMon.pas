unit UDirMon;

interface

uses
  SysUtils, Windows, Classes;

type
  {Тип записи с данными об изменении файла (или папки).
  http://msdn.microsoft.com/en-us/library/windows/desktop/aa364391%28v=vs.85%29.aspx}
  FILE_NOTIFY_INFORMATION = packed record
    //Смещение (в байтах) до следующей записи типа FILE_NOTIFY_INFORMATION.
    NextEntryOffset,
    //Код действия (изменения).
    Action,
    //Размер области в байтах, в которой записано имя файла.
    FileNameLength : Integer;
    //Первый символ в имени файла. Имя файла представлено массивом двухбайтных символов.
    FileName : WideChar;
  end;
  TPFni = ^FILE_NOTIFY_INFORMATION;

  TDmThr = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    //Путь к папке, которая будет наблюдаться.
    FPath : String;
    //Описатель окна, которое требуется извещать о поступлении новых данных в буфер FBuff.
    FHNotify : THandle;
    //Номер сообщения, которое должно отсылаться окну FHNotify.
    FWMNotify : Cardinal;
    //Мьютекс для рассинхронизации доступа потоков к прикладному буферу FBuff.
    FHMtxBuff : THandle;
    {Прикладной буфер. В него будут записываться сведения об отслеживаемых
    изменениях в папках и файлах. Данные этого буфера будет использовать
    основной поток.}
    FBuff : array of Byte;
    //Размер значимых данных в прикладном буфере (в байтах).
    FBuffSize : Cardinal;
    //Код ошибки, которая может произойти при вызове API функций.
    FErr : Integer;
  end;

implementation

{ TThr }

procedure TDmThr.Execute;
const
  {Размер буефера в байтах. Это размер того буфера, в который система будет
  записывать сведения об отслеживаемых изменениях.
  Наибольший допустимый размер такого буефера = High(Word) = 65536 байт.}
  SizeBuff = High(Word);
var
  hDir, hEv : THandle;
  Buff : array of Byte;
  SizeRet, ResWait : Cardinal;
  Ovlp : _OVERLAPPED;
  PFni : TPFni;
begin
  //Проверки.
  if FHNotify = 0 then
    raise Exception.Create('Монитор. Описатель получателя сообщений не задан!');
  if FWMNotify = 0 then
    raise Exception.Create('Монитор. Номер сообщения не задан!');
  if FHMtxBuff = 0 then
    raise Exception.Create('Монитор. Мютекс не определён!');
  //Инициализация.
  hEv := 0;
  //Получаем описатель папки.
  hDir := CreateFile(PChar(FPath), GENERIC_READ, FILE_SHARE_READ
    or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
  //Проверка описателя.
  if hDir = INVALID_HANDLE_VALUE then begin
    FErr := GetLastError;
    Exit;
  end;
  try
    {Выделяем память для буфера. В этот буфер система будет записывать
    сведения об отслеживаемых изменениях в файлах и папках.}
    SetLength(Buff, SizeBuff);
    {Создаём сиглальный объект (объект-событие) с автоматическим сбросом,
    в безсигнальном (занятом) состоянии и получаем его описатель.}
    hEv := CreateEvent(nil, False, False, nil);
    if hEv = 0 then begin
      FErr := GetLastError;
      Exit; //При этом управление перейдёт в раздел finally - end.
    end;
    //Записываем описатель объекта-события в структуру Ovlp.
    Ovlp.hEvent := hEv;
    //Наблюдение.
    while not Terminated do begin
      {Наблюдение за папкой.
      При первом вызове ReadDirectoryChangesW() система создаёт буфер в памяти,
      связывает его с файловым описателем hDir и запускает наблюдение. В этот
      буфер система записывает сведения об изменениях файлов и папок. Система
      будет продолжать наблюдение до момента, когда будет освобождён описатель
      папки - через вызов CloseHandle(hDir).
      При каждом вызове ReadDirectoryChangesW() (и при первом вызове - тоже)
      подаётся запрос на получение сведений об изменениях и на запись их в
      буфер Buff.
      Если произошли отслеживаемые изменения и система записала сведения о них
      в буфер Buff, то система записывает в структуру Ovlp сведения о переданных
      данных и устанавливает объект-событие Ovlp.hEvent в сигнальное состояние.}
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
      {Здесь происходит следующее. На каждой итерации цикла выполняется вызов
      WaitForSingleObject(). Эта функция 500 миллисекунд ждёт момента, когда
      объект-событие Ovlp.hEvent перейдёт в сигнальное состояние. Если за это
      время объект-событие не перешёл в сигнальное состояние, то функция
      WaitForSingleObject() завершит свою работу и возвратит значение: WAIT_TIMEOUT.
      Если WaitForSingleObject() обнаружит, что объект-событие оказался
      в сигнальном состоянии, то WaitForSingleObject() завершит свою работу
      и возвратит значение: WAIT_OBJECT_0. В этом случае система автоматически
      переведёт объект-событие в несигнальное состояние (состояние ожидаения).
      В конце каждой итерации цикла выполняется проверка флага завершения потока
      Terminated и результат вызова функции WaitForSingleObject().
      Выход из цикла происходит, если Terminated = True - т. е., установлен флаг
      завершения потока. Или если ResWait <> WAIT_TIMEOUT - т. е., функция
      WaitForSingleObject() обнаружила, что объект-событие перешёл в сигнальное
      состояние или, если произошла ошибка.}
      repeat
        ResWait := WaitForSingleObject(hEv, 500);
      until Terminated or (ResWait <> WAIT_TIMEOUT);

      {Если объект-событие перешёл в сигнальное состояние. В этом случае нам надо
      забрать данные из буфера Buff.}
      if ResWait = WAIT_OBJECT_0 then begin
        {Опередяем размер данных, которые записаны в буфер Buff. Для этого
        вызываем GetOverlappedResult(). Размер будет записан в переменную SizeRet.
        Через четвертый параметр задаётся режим ожидания. True - ожидать до
        момента, когда операция передачи данных завершится. False - не ожидать.
        В нашем случае известно, что операция уже завершилась, поэтому четвёртый
        параметр устанавливаем в False.}
        if not GetOverlappedResult(hDir, Ovlp, SizeRet, False) then begin
          FErr := GetLastError;
          Exit;
        end;
        {Забираем данные из буфера Buff и записываем их в прикладной буфер.
        Нам надо обеспечить режим, при котором доступ к прикладному буферу
        получает только один поток - основной или дополнительный. Пока буфер
        используется первым потоком, второй поток должен ожидать. Когда
        первый поток завершит работу с буфером, тогда доступ к буферу может
        получить второй поток. И теперь уже первый поток, если пожелает обратиться
        к буферу, должен будет ожидать, пока буфер используется вторым потоком.
        Для обеспечения такого режима применим мьютекс (MUTEX). Мьютекс создан
        в основном потоке и его описатель записан в поле TThr.FHMtxBuff.
        Ждём момента, когда мьютекс станет свободным и захватываем его.}
        if WaitForSingleObject(FHMtxBuff, INFINITE) = WAIT_OBJECT_0 then begin
          //Если требуется, увеличиваем размер прикладного буфера.
          if Length(FBuff) < Integer(FBuffSize + SizeRet) then
            SetLength(FBuff, FBuffSize + SizeRet);
          //Добавляем в прикладной буфер новые данные.
          CopyMemory(@FBuff[FBuffSize], @Buff[0], SizeRet);
          {Если перед добавлением новых данных буфер был непустым, то в последней
          записи прежних данных исправляем сведения о смещении.}
          if FBuffSize > 0 then begin
            //Указатель на первую запись в буфере.
            PFni := @FBuff[0];
            //Ищем указатель на последнюю запись в прежних данных.
            while PFni^.NextEntryOffset <> 0 do
              Inc(PByte(PFni), PFni^.NextEntryOffset);
            //В найденной записи исправляем сведения о смещении.
            PFni^.NextEntryOffset := Integer(@FBuff[FBuffSize]) - Integer(PFni);
          end;
          {Размер значимых данных в прикладном буфере теперь увеличился
          на SizeRet байт.}
          Inc(FBuffSize, SizeRet);
          //Отпускаем мьютекс.
          ReleaseMutex(FHMtxBuff);
          {Посылаем диспетчеру сообщение, означающее, что в прикладной буфер
          добавлены новые данные.}
          PostMessage(FHNotify, FWMNotify, 0, 0);
        end;
      //Если произошла ошибка.
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
