unit uFileTools;

//////////////////////////////////////
///  Lina File Tools Unit          ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  Classes, SysUtils, ShellAPI, Forms, Windows, Math, Graphics,
  {$IFNDEF NO_GENERIC}
    Generics.Collections,
  {$ENDIF}
  { Andere Package-Units }
  uBase, uSysTools;

type
  { Fehlermeldungen }
  EDriveNoExist = class(Exception);
  EFileNoExist = class(Exception);
  EMissingTypeDesc = class(Exception);
  EMissingExts = class(Exception);
  EInvalidStyle = class(Exception);
  ENoGetFileOwner = class(Exception);
  EDllFileNoExist = class(Exception);
  EDllMethodNoExist = class(Exception);

  { Hilfsklassen }
  TFileExecuteMode = (feOpen,feEdit,feExplore,feFind,fePrint,feProperties,feRunAs,feRunAsUser);
  TFileNameStyles = set of (fnDirectory,fnExtension);
  TFileAttributes = set of (faReadOnly,faHidden,faSystem,faArchive,faTemporary);
  TRecursionDepth = (rdNone, rdSingle, rdMultiple);
  TInvalidFileName = String[4];
  TInvalidFileNames = array[1..22] of TInvalidFileName;

  { Hauptklassen }
  TDllFile = record
    FileName: String;
    Handle: THandle;
  end;

  TDllManager = class
  private
    { Private-Deklarationen }
    FFileName: String;
    FFiles: array of TDllFile;
    function GetFiles(Index: Integer): TDllFile;
    function GetFileCount: Integer;
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
    property Files[Index: Integer]: TDllFile read GetFiles;
    property FileCount: Integer read GetFileCount;
    procedure Load(const FileName: String);
    procedure Close(DLL: TDllFile); overload;
    procedure Close(FileName: String); overload;
    procedure Close(Handle: THandle); overload;
    function GetMethod(Name: String): Pointer;
    function GetProcedure(Name: String): TProcedure;
    function MethodExists(Name: String): Boolean;
    function LibraryExists(FileName: String): Boolean;
  end;

  TWinFileInfo = class
  private
    { Private-Deklarationen }
    FFileName: String;
  public
    { Public-Deklarationen }
    constructor Create(AFileName: String);
    destructor Destroy; override;
    property FileName: String read FFileName;
{    function GetSignature: String;
    function GetVersionStructure: Integer;
    function GetVersionFileMS: Integer;
    function GetVersionFileLS: Integer;
    function GetVersionProductMS: Integer;
    function GetVersionProductLS: Integer;    }
  end;

  TWinDrive = class
  private
    { Private-Deklarationen }
    FDriveChar: Char;
  public
    { Public-Deklarationen }
    constructor Create(ADriveChar: Char);
    destructor Destroy; override;
    property DriveChar: Char read FDriveChar;
    function GetSpaceTotal: Int64;
    function GetSpaceFree: Int64;
  end;

  TWinFile = class
  private
    { Private-Deklarationen }
    FFileName: String;
    FExecuteMode: TFileExecuteMode;
    FDrive: TWinDrive;
    FInfo: TWinFileInfo;
  public
    { Public-Deklarationen }
    constructor Create(AFileName: String);
    destructor Destroy; override;
    property FileName: String read FFileName;
    property ExecuteMode: TFileExecuteMode read FExecuteMode write FExecuteMode;
    property Drive: TWinDrive read FDrive write FDrive;
    property Info: TWinFileInfo read FInfo write FInfo;
    function GetExtension(WithDot: Boolean = True): String;     //ExtractFileExt()
    function GetPath: String;                                   //ExtractFilePath()
    function GetDir: String;                                    //ExtractFileDir()
    function GetFileName(WithExt: Boolean = True): String;      //ExtractFileName()
    function GetFolderName: String;                             //ExtractFileFolder()
    function GetSize: Int64;                                    //GetFileSize()
    function GetIcon(Index: Integer = 0): TIcon;                                    //GetFileIcon(0)
    //function GetVersion: Extended;
    function GetAttributes: TFileAttributes;                    //GetFileAttributes();
    function GetOwner: String;
    function GetModified: TDateTime;                            //GetFileModified()
    function GetCreated: TDateTime;                             //GetFileCreated()
    function GetAccessed: TDateTime;                            //GetFileAccessed()
    function Execute: Boolean;                                  //ExecuteFile()
    function SafeExecute: Boolean;                              //.............
  end;

  TWinFileArray = array of TWinFile;

  procedure InitializeInvalidNames;
  function ValidFileName(const FileName: TInvalidFileName; const InvalidFileNames: TInvalidFileNames): Boolean;
  function ValidFileNameStr(const FileName: String; const InvalidFileNames: TInvalidFileNames): Boolean;
  function StrIsPath(const S: String): Boolean;
  function FEModeToPChar(FEMode: TFileExecuteMode): PChar;
  procedure EnsureDirDelimeter(var Dir: String);
  function ExecuteFile(FileName: String; ExecMode: TFileExecuteMode = feOpen;
    InDir: Boolean = False): Boolean;
  function ExtractFileFolder(FileName: String): String;
  procedure ListFiles(Dir: String; out OutList: TStrings; FileExts: array of String;
    NameStyles: TFileNameStyles = []; RecDepth: TRecursionDepth = rdNone); overload;
  procedure ListFiles(Dir: String; out OutList: TStrings; FileExt: String;
    NameStyles: TFileNameStyles = []; RecDepth: TRecursionDepth = rdNone); overload;
  procedure ListFolders(Dir: String; var OutList: TStrings;
    NameStyles: TFileNameStyles = []; RecDepth: TRecursionDepth = rdNone);
  function ExtractDriveChar(const FileName: String): Char;
  function DriveCharToFileDir(DriveChar: Char): ShortString;
  function DriveCharToFilePath(DriveChar: Char): ShortString;
  function DriveByteToDriveChar(DriveByte: Byte): Char;
  function DriveCharToDriveByte(DriveChar: Char): Byte;
  function DriveExists(DriveByte: Byte): Boolean;
  function GetDriveTypeChar(DriveByte: Byte): UINT;
  function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
  function ConvertFileSize(const InputSize: Extended; ConvertFactor: ShortInt = -1): Extended; overload;
  function ConvertFileSize(const InputSize: Int64; ConvertFactor: ShortInt = -1): Int64; overload;
  function GetFileSize(FileName: String): Int64;
  function GetFileModified(FileName: String): TDateTime;
  function GetFileCreated(FileName: String): TDateTime;
  function GetFileAccessed(FileName: String): TDateTime;
  function GetFileAttributes(FileName: String): TFileAttributes;
  function GetFileOwner(FileName: String): String;
  function GetFileIcon(FileName: String; Index: Word = 0): TIcon;

const
  PathDelims = [PathDelim,'/'];
  MAX_PATH_DS = MAX_PATH - 1;        //Max. Länge von Dateiname ohne #0-Terminal
  { Ungültige Dateinamen-Zeichen }
  InvalidFilePathChars = ['*','?','"','<','>','|'];
  InvalidFileNameChars = InvalidFilePathChars + ['\','/',':'];
  { Umrechnungs-Faktoren }
  TB_TO_B  =  4;
  GB_TO_B  =  3;
  TB_TO_KB =  3;
  MB_TO_B  =  2;
  GB_TO_KB =  2;
  TB_TO_MB =  2;
  KB_TO_B  =  1;
  MB_TO_KB =  1;
  GB_TO_MB =  1;
  TB_TO_GB =  1;
   B_TO_KB = -1;
  KB_TO_MB = -1;
  MB_TO_GB = -1;
  GB_TO_TB = -1;
   B_TO_MB = -2;
  KB_TO_GB = -2;
  MB_TO_TB = -2;
   B_TO_GB = -3;
  KB_TO_TB = -3;
   B_TO_TB = -4;
  { Dateierweiterungen für ListFiles() }
  FXT_ANY = '*.*';
  FXT_EXEC: array [0..2] of String = ('*.exe','*.com','*.scr');
  FXT_TEXT: array [0..1] of String = ('*.txt','*.rtf');
  FXT_IMAGE: array [0..6] of String = ('*.jpg','*.jpeg','*.png','*.tif','*.tiff','*.bmp','*.gif');
  FXT_SOURCE: array [0..3] of String = ('*.pas','*.dpr','*.dpk','*.dfm');
  FXT_OFFICE: array [0..2] of String = ('*.doc','*.xls','*.ppt');
  FXT_OFFICEX: array [0..2] of String = ('*.docx','*.xlsx','*.pptx');
  FXT_INSTALL: array [0..1] of String = ('*.msi','*.msu');
  FXT_DISK: array [0..2] of String = ('*.img','*.iso','*.bin');
  FXT_ARCHIVE: array [0..4] of String = ('*.zip','*.rar','*.7z','*.tar','*.gz');

var
  InvalidFileNames: TInvalidFileNames;

implementation

procedure InitializeInvalidNames;
begin
  InvalidFileNames[01] := 'CON';
  InvalidFileNames[02] := 'PRN';
  InvalidFileNames[03] := 'AUX';
  InvalidFileNames[04] := 'NUL';
  { COM... }
  InvalidFileNames[05] := 'COM1';
  InvalidFileNames[06] := 'COM2';
  InvalidFileNames[07] := 'COM3';
  InvalidFileNames[08] := 'COM4';
  InvalidFileNames[09] := 'COM5';
  InvalidFileNames[10] := 'COM6';
  InvalidFileNames[11] := 'COM7';
  InvalidFileNames[12] := 'COM8';
  InvalidFileNames[13] := 'COM9';
  { LPT... }
  InvalidFileNames[14] := 'LPT1';
  InvalidFileNames[15] := 'LPT2';
  InvalidFileNames[16] := 'LPT3';
  InvalidFileNames[17] := 'LPT4';
  InvalidFileNames[18] := 'LPT5';
  InvalidFileNames[19] := 'LPT6';
  InvalidFileNames[20] := 'LPT7';
  InvalidFileNames[21] := 'LPT8';
  InvalidFileNames[22] := 'LPT9';
end;

function ValidFileName(const FileName: TInvalidFileName;
 const InvalidFileNames: TInvalidFileNames): Boolean;
var
  Index: 1..22;
begin
  InitializeInvalidNames;
  Result := True;
  for Index := Low(InvalidFileNames) to High(InvalidFileNames) do
  begin
    if InvalidFileNames[Index] = FileName then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function ValidFileNameStr(const FileName: String;
 const InvalidFileNames: TInvalidFileNames): Boolean;
var
  Index: 1..4;
  TmpIFN: TInvalidFileName;
begin
  Result := True;
  if Length(FileName) > 4 then
  begin
    Exit;
  end;
  for Index := 1 to 4 do
  begin
    TmpIFN := TmpIFN + FileName[Index];
  end;
  Result := ValidFileName(TmpIFN,InvalidFileNames);
end;

function StrIsPath(const S: String): Boolean;
{ Prüft, ob ein Dateipfad unter Windows-Systemen gültig ist.
  An dieser stelle sei gesagt: Der Rückgabewert dieser Funktion ist lediglich
  ein Anhaltspunkt und keine Garantie!
  Außerdem wird nicht geprüft, ob die Datei existiert. }
var
  Index: 1..MAX_PATH_DS;          //Integer[1..259]
  SLength: SmallInt;
  NoPathDelim: Boolean;
  NeedPathDelim: Boolean;
  FileName: ShortString;
begin
  Result := True;
  SLength := Length(S);
  if SLength = 0 then
  begin
    Exit;
  end;
  if SLength > MAX_PATH_DS then
  begin
    Result := False;
    Exit;
  end;
  { Prüfen, ob Dateiname ungültig ist }
  FileName := '';
  for Index := SLength downto 1 do
  begin
    if S[Index] in PathDelims then
    begin
      Break;
    end else
    begin
      FileName := S[Index] + FileName;
    end;
  end;
  if not ValidFileNameStr(FileName,InvalidFileNames) then
  begin
    Result := False;
    Exit;
  end;
  { Parse... }
  NoPathDelim := True;
  NeedPathDelim := False;
  for Index := 1 to SLength do
  begin
    { -> Doppel-Slash verhindern }
    if NoPathDelim then
    begin
      if S[Index] in PathDelims then
      begin
        Result := False;
        Exit;
      end else
      begin
        NoPathDelim := False;
      end;
    end else
    begin
      NoPathDelim := (S[Index] in PathDelims);
    end;
    { -> Drive-Char validieren }
    if NeedPathDelim then
    begin
      if S[Index] in PathDelims then
      begin
        NeedPathDelim := False;
      end else
      begin
        Result := False;
        Exit;
      end;
    end;
    if S[Index] = DriveDelim then
    begin
      if Index = 2 then
      begin
        NeedPathDelim := True;
      end else
      begin
        Result := False;
        Exit;
      end;
    end;
    { -> Auf ungültige Zeichen prüfen }
    if S[Index] in InvalidFilePathChars then
    begin
      Result := False;
      Exit;
    end;
  //For
  end;
end;

function FEModeToPChar(FEMode: TFileExecuteMode): PChar;
begin
  case FEMode of
    feOpen: Result := 'open';
    feEdit: Result := 'edit';
    feExplore: Result := 'explore';
    feFind: Result := 'find';
    fePrint: Result := 'print';
    feProperties: Result := 'properties';
    feRunAs: Result := 'runas';
    feRunAsUser: Result := 'runasuser';
    else Result := nil;
  end;
end;

procedure EnsureDirDelimeter(var Dir: String);
begin
  if (Dir[Length(Dir)] <> '\') and (Dir[Length(Dir)] <> '/') then
  begin
    Dir := Dir + '\';
  end else
  begin
    while Length(Dir) >= 1 do
    begin
      if (Dir[Length(Dir) - 1] = '\') or (Dir[Length(Dir) - 1] = '/') then
      begin
        Delete(Dir,Length(Dir) - 1,1);
      end else
      begin
        Break;
      end;
    end;
  end;
end;

procedure ListFiles(Dir: String; out OutList: TStrings; FileExts: array of String;
  NameStyles: TFileNameStyles = []; RecDepth: TRecursionDepth = rdNone);
var
  SRec: TSearchRec;
  ExtIndex: Integer;
begin
  EnsureDirDelimeter(Dir);
  if Length(FileExts) < 1 then
  begin
    raise EMissingExts.Create('Missing file extensions');
  end;
  if Assigned(OutList) then
  begin
    OutList.Clear;
  end else
  begin
    OutList := TStringList.Create;
  end;
  for ExtIndex := Low(FileExts) to High(FileExts) do
  begin
    if FindFirst(Dir + '*.*',faAnyFile,SRec) = 0 then
    begin
      repeat
        if (SRec.Name <> '.') and (SRec.Name <> '..') then
        begin
          if ((SRec.Attr and faDirectory) = faDirectory) then
          begin
            case RecDepth of
              rdSingle: ListFiles(Dir + SRec.Name,OutList,FileExts,NameStyles);
              rdMultiple: ListFiles(Dir + SRec.Name,OutList,FileExts,NameStyles,rdMultiple);
            end;
          end else
          begin
            if ((ChangeFileExt(SRec.Name,ExtractFileExt(FileExts[ExtIndex])) = SRec.Name) or
                (ExtractFileExt(FileExts[ExtIndex]) = '.*')) then
            begin
              OutList.Add(SRec.Name);
              if fnDirectory in NameStyles then
              begin
                OutList.Strings[OutList.Count - 1] := Dir + OutList.Strings[OutList.Count - 1];
              end;
              if not (fnExtension in NameStyles) then
              begin
                OutList.Strings[OutList.Count - 1] := ChangeFileExt(OutList.Strings[OutList.Count - 1],'');
              end;
            end;
          end;
        end;
      until FindNext(SRec) <> 0;
      SysUtils.FindClose(SRec);
    end;
  end;
end;

procedure ListFiles(Dir: String; out OutList: TStrings; FileExt: String;
  NameStyles: TFileNameStyles = []; RecDepth: TRecursionDepth = rdNone);
begin
  ListFiles(Dir,OutList,[FileExt],NameStyles,RecDepth);
end;

procedure ListFolders(Dir: String; var OutList: TStrings;
  NameStyles: TFileNameStyles = []; RecDepth: TRecursionDepth = rdNone);
var
  SRec: TSearchRec;
begin
  EnsureDirDelimeter(Dir);
  if FindFirst(Dir + '*.*',faAnyFile,SRec) = 0 then
  begin
    repeat
      if ((SRec.Attr and faDirectory) = faDirectory) and (SRec.Name <> '.') and (SRec.Name <> '..') then
      begin
        if fnDirectory in NameStyles then
        begin
          OutList.Add(Dir + SRec.Name);
        end else
        begin
          OutList.Add(SRec.Name);
          case RecDepth of
            rdSingle: ListFolders(Dir + SRec.Name,OutList,NameStyles);
            rdMultiple: ListFolders(Dir + SRec.Name,OutList,NameStyles,rdMultiple);
          end;
        end;
      end;
    until FindNext(SRec) <> 0;
    SysUtils.FindClose(SRec);
  end;
  if fnExtension in NameStyles then
  begin
    raise EInvalidStyle.Create('The extension file name style is invalid for directory names and has been ignored');
  end;
end;

function ExecuteFile(FileName: String; ExecMode: TFileExecuteMode = feOpen;
  InDir: Boolean = False): Boolean;
begin
  Result := True;
  try
    if InDir then
    begin
      ShellExecute(Application.Handle,FEModeToPChar(ExecMode),PChar(FileName),nil,PChar(ExtractFileDir(FileName)),SW_NORMAL);
    end else
    begin
      ShellExecute(Application.Handle,FEModeToPChar(ExecMode),PChar(FileName),nil,nil,SW_NORMAL);
    end;
  except
    Result := False;
  end;
end;

function ExtractFileFolder(FileName: String): String;
begin
  Result := ExtractFileName(ExtractFileDir(FileName));        //Name d. übergeord. Ordners
end;

function ExtractDriveChar(const FileName: String): Char;
begin
  Result := FileName[1];
  if Length(FileName) >= 2 then
  begin
    if FileName[2] <> DriveDelim then
    begin
      Result := #0;
    end else
    begin
      if Length(FileName) >= 3 then
      begin
        if not (FileName[3] in PathDelims) then
        begin
          Result := #0;
        end;
      end;
    end;
  end;
end;

function DriveCharToFileDir(DriveChar: Char): ShortString;
begin
  Result := DriveChar + DriveDelim;
end;

function DriveCharToFilePath(DriveChar: Char): ShortString;
begin
  Result := DriveCharToFileDir(DriveChar) + PathDelim;
end;

function DriveByteToDriveChar(DriveByte: Byte): Char;
begin
  Result := Chr(DriveByte + Ord('A'));
end;

function DriveCharToDriveByte(DriveChar: Char): Byte;
begin
  Result := Ord(DriveChar) - Ord('A');
end;

function DriveExists(DriveByte: Byte): Boolean;
begin
  Result := (GetLogicalDrives and (1 shl DriveByte) <> 0);
end;

function GetDriveTypeChar(DriveByte: Byte): UINT;
begin
  Result := GetDriveType(PChar(String(DriveCharToFilePath(DriveByteToDriveChar(DriveByte)))));
end;

function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
var
  LocalTime: TFileTime;
  SysTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime,LocalTime);
  FileTimeToSystemTime(LocalTime,SysTime);
  Result := SystemTimeToDateTime(SysTime);
end;

function ConvertFileSize(const InputSize: Extended; ConvertFactor: ShortInt = -1): Extended;
begin
  { Sollte verwendet werden mit den Umrechnungs-Faktoren, die in der globalen
    "const"-Section deklariert wurden. }
  Result := InputSize * IntPower(1024,ConvertFactor);
end;

function ConvertFileSize(const InputSize: Int64; ConvertFactor: ShortInt = -1): Int64;
begin
  { Sollte verwendet werden mit den Umrechnungs-Faktoren, die in der globalen
    "const"-Section deklariert wurden. }
  Result := Round(InputSize * IntPower(1024,ConvertFactor));
end;

function GetFileSize(FileName: String): Int64;
var
  FileHandle: THandle;
  FindData: TWIN32FINDDATA;
begin
  Result := -1;
  if FileExists(FileName) then
  begin
    FileHandle := FindFirstFile(pchar(FileName),FindData);
    if FileHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;
    Result := (FindData.nFileSizeHigh * (MAXDWORD)) + FindData.nFileSizeLow;
    Windows.FindClose(FileHandle);
  end;
end;

function GetFileModified(FileName: String): TDateTime;
var
  FileHandle: THandle;
  FindData: TWIN32FINDDATA;
begin
  Result := StrToTime('00.00.0000 00:00:00');
  if FileExists(FileName) then
  begin
    FileHandle := FindFirstFile(PChar(FileName),FindData);
    if FileHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;
    Result := FileTimeToDateTime(FindData.ftLastWriteTime);
    Windows.FindClose(FileHandle);
  end;
end;

function GetFileCreated(FileName: String): TDateTime;
var
  FileHandle: THandle;
  FindData: TWIN32FINDDATA;
begin
  Result := StrToTime('00.00.0000 00:00:00');
  if FileExists(FileName) then
  begin
    FileHandle := FindFirstFile(PChar(FileName),FindData);
    if FileHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;
    Result := FileTimeToDateTime(FindData.ftCreationTime);
    Windows.FindClose(FileHandle);
  end;
end;

function GetFileAccessed(FileName: String): TDateTime;
var
  FileHandle: THandle;
  FindData: TWIN32FINDDATA;
begin
  Result := StrToTime('00.00.0000 00:00:00');
  if FileExists(FileName) then
  begin
    FileHandle := FindFirstFile(PChar(FileName),FindData);
    if FileHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;
    Result := FileTimeToDateTime(FindData.ftLastAccessTime);
    Windows.FindClose(FileHandle);
  end;
end;

function GetFileAttributes(FileName: String): TFileAttributes;
var
  FileHandle: THandle;
  FindData: TWIN32FINDDATA;
begin
  Result := [];
  if FileExists(FileName) then
  begin
    FileHandle := FindFirstFile(PChar(FileName),FindData);
    if FileHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY) = FILE_ATTRIBUTE_READONLY then
    begin
      Result := Result + [faReadOnly];
    end;
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN) = FILE_ATTRIBUTE_HIDDEN then
    begin
      Result := Result + [faHidden];
    end;
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM) = FILE_ATTRIBUTE_SYSTEM then
    begin
      Result := Result + [faSystem];
    end;
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE) = FILE_ATTRIBUTE_ARCHIVE then
    begin
      Result := Result + [faArchive];
    end;
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_TEMPORARY) = FILE_ATTRIBUTE_TEMPORARY then
    begin
      Result := Result + [faTemporary];
    end;
    Windows.FindClose(FileHandle);
  end;
end;

function GetFileOwner(FileName: String): String;
var
  SecDescr: PSecurityDescriptor;
  SizeNeeded, SizeNeeded2: DWORD;
  OwnerSID: PSID;
  OwnerDefault: BOOL;
  OwnerName, DomainName: PChar;
  OwnerType: SID_NAME_USE;
begin
  GetMem(SecDescr,1024);
  GetMem(OwnerSID,SizeOf(PSID));
  GetMem(OwnerName,1024);
  GetMem(DomainName,1024);
  try
    if GetFileSecurity(PChar(FileName),OWNER_SECURITY_INFORMATION,SecDescr,1024,SizeNeeded) then
    begin
      if GetSecurityDescriptorOwner(SecDescr,OwnerSID,OwnerDefault) then
      begin
        SizeNeeded := 1024;
        SizeNeeded2 := 1024;
        if LookupAccountSID(nil,OwnerSID,OwnerName,SizeNeeded,DomainName,SizeNeeded2,OwnerType) then
        begin
          Result := OwnerName + '@' + DomainName;
        end else
        begin
          raise ENoGetFileOwner.Create('Could not determine the file owner');
        end;
      end else
      begin
        raise ENoGetFileOwner.Create('Could not determine the file owner');
      end;
    end else
    begin
      raise ENoGetFileOwner.Create('Could not determine the file owner');
    end;
  finally
    FreeMem(SecDescr);
    FreeMem(OwnerName);
    FreeMem(DomainName);
  end;
end;

function GetFileIcon(FileName: String; Index: Word = 0): TIcon;
begin
  Result := TIcon.Create;
  Result.Handle := ExtractAssociatedIcon(HInstance, PChar(FileName), Index);
end;

{ ----------------------------------------------------------------------------
  TDllManager
  ---------------------------------------------------------------------------- }

constructor TDllManager.Create;
begin
  inherited;
  SetLength(FFiles,0);
end;

destructor TDllManager.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FileCount - 1 do
  begin
    Close(Files[Index]);
  end;
  inherited;
end;

function TDllManager.GetFiles(Index: Integer): TDllFile;
begin
  Result := FFiles[Index];
end;

function TDllManager.GetFileCount: Integer;
begin
  Result := Length(FFiles);
end;

procedure TDllManager.Load(const FileName: String);
var
  Handle: THandle;
  DLL: TDllFile;
begin
  SetLength(FFiles,Length(FFiles) + 1);
  Handle := LoadLibrary(PChar(FileName));
  if Handle = 0 then
  begin
    raise EDllFileNoExist.Create('Library "' + FileName + '" not found');
  end else
  begin
    Dll.FileName := FileName;
    Dll.Handle := Handle;
    FFiles[High(FFiles)] := DLL;
  end;
end;

procedure TDllManager.Close(DLL: TDllFile);
var
  Index: Integer;
begin
  for Index := 0 to FileCount - 1 do
  begin
    if (Files[Index].FileName = DLL.FileName) and (Files[Index].Handle = DLL.Handle) then
    begin
      Break;
    end;
  end;
  while Index < FileCount - 1 do
  begin
    FFiles[Index] := Files[Index + 1];
    Inc(Index);
  end;
  if Index = FileCount then
  begin
    raise EDllFileNoExist.Create('Library [' + IntToStr(DLL.Handle) + '] "' + DLL.FileName + '" not loaded');
  end else
  begin
    SetLength(FFiles,Length(FFiles) - 1);
  end;
end;

procedure TDllManager.Close(FileName: String);
var
  Index: Integer;
begin
  for Index := 0 to FileCount - 1 do
  begin
    if Files[Index].FileName = FileName then
    begin
      Break;
    end;
  end;
  while Index < FileCount - 1 do
  begin
    FFiles[Index] := Files[Index + 1];
    Inc(Index);
  end;
  if Index = FileCount then
  begin
    raise EDllFileNoExist.Create('Library "' + FileName + '" not loaded');
  end else
  begin
    SetLength(FFiles,Length(FFiles) - 1);
  end;
end;

procedure TDllManager.Close(Handle: THandle);
var
  Index: Integer;
begin
  for Index := 0 to FileCount - 1 do
  begin
    if Files[Index].Handle = Handle then
    begin
      Break;
    end;
  end;
  while Index < FileCount - 1 do
  begin
    FFiles[Index] := Files[Index + 1];
    Inc(Index);
  end;
  if Index = FileCount then
  begin
    raise EDllFileNoExist.Create('Library [' + IntToStr(Handle) + '] not loaded');
  end else
  begin
    SetLength(FFiles,Length(FFiles) - 1);
  end;
end;

function TDllManager.GetMethod(Name: String): Pointer;
begin

end;

function TDllManager.GetProcedure(Name: String): TProcedure;
begin
  Result := TProcedure(GetMethod(Name));
end;

function TDllManager.MethodExists(Name: String): Boolean;
begin
 // Load
end;

function TDllManager.LibraryExists(FileName: String): Boolean;
var
  Handle: THandle;
begin
  Handle := LoadLibrary(PChar(FileName));
  if Handle = 0 then
  begin
    Result := False;
  end else
  begin
    Result := True;
    FreeLibrary(Handle);
  end;
end;

{ ----------------------------------------------------------------------------
  TWinFileInfo
  ---------------------------------------------------------------------------- }

constructor TWinFileInfo.Create(AFileName: String);
begin
  FFileName := AFileName;
  if not FileExists(FFileName) then
  begin
    raise EFileNoExist.Create('File not found: "' + FFileName + '"');
  end;
end;

destructor TWinFileInfo.Destroy;
begin
  //...
  inherited;
end;

{ ----------------------------------------------------------------------------
  TWinDrive
  ---------------------------------------------------------------------------- }

constructor TWinDrive.Create(ADriveChar: Char);
begin
  FDriveChar := ADriveChar;
  if not DirectoryExists(DriveCharToFileDir(FDriveChar)) then
  begin
    raise EDriveNoExist.Create('Drive not found: "' + FDriveChar + '"');
  end;
end;

destructor TWinDrive.Destroy;
begin
  //...
  inherited;
end;

function TWinDrive.GetSpaceTotal: Int64;
var
  RootChars: array[0..4] of Char;
  Root: PChar;
  Dir: String;
  Buffer: Int64;
begin
  RootChars[0] := FDriveChar;
  RootChars[1] := DriveDelim;
  RootChars[2] := PathDelim;
  RootChars[3] := #0;
  Root := RootChars;
  Dir := GetCurrentDir;
  if SetCurrentDir(DriveCharToFilePath(FDriveChar)) then
  begin
    GetDiskFreeSpaceEx(Root,Buffer,Result,nil);
    SetCurrentDir(Dir);
  end else
  begin
    Result := -1;
  end;
end;

function TWinDrive.GetSpaceFree: Int64;
var
  RootChars: array[0..4] of Char;
  Root: PChar;
  Dir: String;
  Buffer: Int64;
begin
  RootChars[0] := FDriveChar;
  RootChars[1] := DriveDelim;
  RootChars[2] := PathDelim;
  RootChars[3] := #0;
  Root := RootChars;
  Dir := GetCurrentDir;
  if SetCurrentDir(DriveCharToFilePath(FDriveChar)) then
  begin
    GetDiskFreeSpaceEx(Root,Result,Buffer,nil);
    SetCurrentDir(Dir);
  end else
  begin
    Result := -1;
  end;
end;

{ ----------------------------------------------------------------------------
  TWinFile
  ---------------------------------------------------------------------------- }

constructor TWinFile.Create(AFileName: String);
begin
  ExecuteMode := feOpen;
  FFileName := AFileName;
  FDrive := TWinDrive.Create(ExtractDriveChar(FFileName));
  FInfo := TWinFileInfo.Create(FFileName);
  if not FileExists(FFileName) then
  begin
    raise EFileNoExist.Create('File not found: "' + FFileName + '"');
  end;
end;

destructor TWinFile.Destroy;
begin
  FDrive.Free;
  FInfo.Free;
  inherited;
end;

function TWinFile.GetExtension(WithDot: Boolean = True): String;
begin
  if WithDot then
  begin
    Result := ExtractFileExt(FFileName);
  end else
  begin
    Result := Copy(ExtractFileExt(FFileName),2,Length(ExtractFileExt(FFileName)) - 1);
  end;
end;

function TWinFile.GetFileName(WithExt: Boolean = True): String;
begin
  if WithExt then
  begin
    Result := ExtractFileName(FFileName);                     //Name + Erweiterung
  end else
  begin
    Result := ExtractFileName(ChangeFileExt(FFileName,''));   //Nur Dateiname
  end;
end;

function TWinFile.GetFolderName: String;
begin
  Result := ExtractFileFolder(FFileName);
end;

function TWinFile.GetSize: Int64;
begin
  Result := GetFileSize(FFileName);
end;

function TWinFile.GetIcon(Index: Integer = 0): TIcon;
begin
  Result := GetFileIcon(FFileName, Index);
end;

function TWinFile.GetPath: String;
begin
  Result := ExtractFilePath(FFileName);                       //Gesamter Ordnerpfad (ohne Dateiname)
end;

function TWinFile.GetDir: String;
begin
  Result := ExtractFileDir(FFileName);
end;

function TWinFile.GetModified: TDateTime;
begin
  Result := GetFileModified(FFileName);
end;

function TWinFile.GetCreated: TDateTime;
begin
  Result := GetFileCreated(FFileName);
end;

function TWinFile.GetAccessed: TDateTime;
begin
  Result := GetFileAccessed(FFileName);
end;

function TWinFile.GetAttributes: TFileAttributes;
begin
  Result := GetFileAttributes(FFileName);
end;

function TWinFile.GetOwner: String;
begin
  Result := GetFileOwner(FFileName);
end;

function TWinFile.Execute: Boolean;
begin
  Result := ExecuteFile(FFileName,ExecuteMode);
end;

function TWinFile.SafeExecute: Boolean;
begin
  Result := ExecuteFile(FFileName,ExecuteMode,True);
end;

end.
