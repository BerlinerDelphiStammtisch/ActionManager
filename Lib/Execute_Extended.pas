unit Execute_Extended;  {Funktionen für Dateihandling}

{$I COMP_OPT.INC}

interface

uses {Delphi} WinProcs, SysUtils, Vcl.Dialogs, Vcl.Forms,
               {$WARNINGS OFF}
               ShellAPI;
               {$IFDEF IDEMODE}
               {$WARNINGS ON}
               {$ENDIF}

const CXMLFileFilter = 'XML-File (*.xml)|*.XML';


{$Region '------------------------- Dateien ausführen ------------------------------------'}
procedure EXTW_ExecTask(const CmdLine:string; CmdShow:word; FWait:boolean);
procedure ExecuteFile(FileName: string; Params: string; WaitUntilFinish: Boolean = false); overload;
procedure ExecuteFile(FileName: string); overload;
function ExecuteDOSFile(FileName: string; Params: string; WaitUntilFinish: Boolean = false): string;
{$EndRegion}

implementation


{$Region '------------------------ Dateien ausführen ------------------------------------'}
procedure EXTW_ExecTask(const CmdLine:string; CmdShow:word; FWait:boolean);
var StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
begin
  { Execution }
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := CmdShow;
  if not CreateProcess(nil,
    PChar(CmdLine),                { pointer to command line string }
    nil,                           { pointer to process security attributes }
    nil,                           { pointer to thread security attributes }
    false,                         { handle inheritance flag }
    CREATE_NEW_CONSOLE or          { creation flags }
    NORMAL_PRIORITY_CLASS,
    nil,                           { pointer to new environment block }
    nil,                           { pointer to current directory name }
    StartupInfo,                   { pointer to STARTUPINFO }
    ProcessInfo) then begin        { pointer to PROCESS_INF }
    ShowMessage(CmdLine+ chr(10)+SysErrorMessage(GetLastError()));
    //raise;
  end
  else if FWait then WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
end;

//Startet auch verknüpfte Anwendung (z.B.: bei Angabe einer Website als "FileName"), ReHo
procedure ExecuteFile(FileName: string; Params: string; WaitUntilFinish: Boolean = false);
var
  exInfo: TShellExecuteInfo;
  Ph: DWORD;
  dir, workdir: String;
begin
  dir    := GetCurrentDir;
  workdir:= ExtractFilePath(FileName);
  SetCurrentDir(workdir);
  FillChar(exInfo, SizeOf(exInfo), 0);
  with exInfo do
  begin
    cbSize      := SizeOf(exInfo);
    fMask       := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
    Wnd         := GetActiveWindow();
    lpVerb      := 'open';
    lpParameters:= PChar(Params);
    lpFile      := PChar(FileName);
    lpDirectory := PChar(workdir);
    nShow       := SW_SHOWNORMAL;
  end;
  if ShellExecuteEx(@exInfo) then
    Ph:= exInfo.HProcess
  else
  begin
    ShowMessage(SysErrorMessage(GetLastError));
    Exit;
  end;
  if WaitUntilFinish then
  begin
    while WaitForSingleObject(ExInfo.hProcess, 50) <> WAIT_OBJECT_0 do
      Application.ProcessMessages;
  end;
  CloseHandle(Ph);
  SetCurrentDir(dir);
end;

procedure ExecuteFile(FileName: string);
begin
  ShellExecute(Application.Handle, nil, PChar(FileName), nil, nil, SW_NORMAL );
end;

function ExecuteDOSFile(FileName: string; Params: string; WaitUntilFinish: Boolean = false): String;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Result := '';
  with SA do begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_NORMAL;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := ExtractFilePath(FileName);
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + FileName+ ' '+ Params),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Result := Result + Buffer;
          end;
        until not WasOK or (BytesRead = 0);
        if WaitUntilFinish then
        begin
          while WaitForSingleObject(PI.hProcess, INFINITE) <> WAIT_OBJECT_0 do
            Application.ProcessMessages;
        end;
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;
{$EndRegion}

end.
