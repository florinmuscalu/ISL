unit UFormManager;
interface
uses Classes, windows, UFileSystem, UGLWinTypes;

function  glwIsActive:boolean; stdcall;
function  glwVersion:byte; stdcall;

{Error Handling}
procedure glwPostError(errCode:cardinal); stdcall;
function  glwGetLastError:cardinal; stdcall;
function  glwTranslateError(errCode:cardinal):pchar; stdcall;
procedure glwAddErrCode(errCode:cardinal; Translation:pchar); stdcall;


{Initialization/Deinitialization}
procedure glwInit(cmdLine:pchar; writeLog:boolean=true); stdcall;
        //need glwInit
        function  glwExeName:pchar; stdcall;
        function  glwAppDir:pchar; stdcall;
        procedure glwDeInit; stdcall;

        {Logging}
        procedure glwAppendLog(s:string); stdcall;

        {events}
        procedure glwSetOnBeforeCreate(event:TOnBeforeCreate=nil); stdcall;
        procedure glwSetOnAfterCreate(event:TOnAfterCreate=nil); stdcall;
        procedure glwSetOnTick(event:TOnTick=nil); stdcall;
        procedure glwSetOnKeyUp(event:TOnKeyUp=nil); stdcall;
        procedure glwSetOnKeyDown(event:TOnKeyDown=nil); stdcall;
        procedure glwSetOnResize(event:TOnResize=nil); stdcall;
        procedure glwSetOnBeforeDestroy(event:TOnBeforeDestroy=nil); stdcall;
        procedure glwSetOnAfterDestroy(event:TOnAfterDestroy=nil); stdcall;
        procedure glwSetOnClose(event:TOnClose=nil); stdcall;
        procedure glwSetOnActivate(event:TOnActivate=nil); stdcall;
        procedure glwSetOnError(event:TOnError=nil); stdcall;
        procedure glwSetOnMouseWheel(event:TOnMouseWheel=nil); stdcall;
        procedure glwSetEventHandler(event:cardinal; proc:TEventProc); stdcall;

        procedure glwGetOnBeforeCreate(var event:TOnBeforeCreate); stdcall;
        procedure glwGetOnAfterCreate(var event:TOnAfterCreate); stdcall;
        procedure glwGetOnTick(var event:TOnTick); stdcall;
        procedure glwGetOnKeyUp(var event:TOnKeyUp); stdcall;
        procedure glwGetOnKeyDown(var event:TOnKeyDown); stdcall;
        procedure glwGetOnResize(var event:TOnResize); stdcall;
        procedure glwGetOnBeforeDestroy(var event:TOnBeforeDestroy); stdcall;
        procedure glwGetOnAfterDestroy(var event:TOnAfterDestroy); stdcall;
        procedure glwGetOnClose(var event:TOnClose); stdcall;
        procedure glwGetOnActivate(var event:TOnActivate); stdcall;
        procedure glwGetOnError(var event:TOnError); stdcall;
        procedure glwGetOnMouseWheel(var event:TOnMouseWheel); stdcall;
        procedure glwGetEventHandler(event:cardinal; var proc:TEventProc); stdcall;

        {Pak file}
        function  glwGetFile(filename:pchar; out size:cardinal):Pointer; stdcall;
        function  glwGetFileAsStream(filename:pchar; out size:cardinal):TMemoryStream; stdcall;
        function  glwGetLastErrorFs:pchar; stdcall;
        procedure glwSaveFileFromStream(var S:TStream; filename:string); stdcall;

        {Window Creation, info}
        procedure glwCreateWindow(handle:HWND); stdcall;
                //need glwCreateWindow
                function  glwFPS:cardinal; stdcall;
                function  glwDeltaTime:single; stdcall;
                function  glwIsKeyDown(const key:cardinal):boolean; stdcall;
                function  glwVKtoASCII(const key:integer):integer; stdcall;
                function  glwShiftState:boolean; stdcall;
                procedure glwTick(var d:Single; var f:cardinal);
                function glwWinHandle:HWND;

        procedure UpdateKey(const Key:cardinal; const _on:boolean);
var
 glwWidth, glwHeight:cardinal;
implementation
uses sysutils, messages;
var
 FS:TFileSystem;

 fWriteLog:boolean;

 fActive:boolean=false;

 finished : Boolean=true;
 fLogStartTime:int64=0;
 fCmdLine:string='';
 fExeName:string='';
 fAppDir:string='';
 h_Wnd:HDC=0;
 delta:single=0;
 fLastTime:int64=0;
 freq:single=0;
 fFPS:cardinal=0;
 FPSCount:cardinal=0;
 fEventList:array of record event:cardinal; proc:TEventProc; end;
 fEventListCount:integer=0;
 keys:array[0..255]of boolean;

 {events}
 OnBeforeCreate:TOnBeforeCreate=nil;
 OnAfterCreate:TOnAfterCreate=nil;
 OnTick:TOnTick=nil;
 OnKeyUp:TOnKeyUp=nil;
 OnKeyDown:TOnKeyDown=nil;
 OnResize:TOnResize=nil;
 OnBeforeDestroy:TOnBeforeDestroy=nil;
 OnAfterDestroy:TOnAfterDestroy=nil;
 OnClose:TOnClose=nil;
 OnActivate:TOnActivate=nil;
 OnError:TOnError=nil;
 OnMouseWheel:TOnMouseWheel=nil;
 {error handling}
 fLastError:cardinal;
 fErrors:array of record code:cardinal; translation:string; end;

function glwVersion:byte;
begin
 result:=1;
end;


procedure glwPostError(errCode:cardinal);
begin
 if fLastError=0 then begin
        fLastError:=errCode;
        glwAppendLog('Error no: '+inttostr(errCode));
        if assigned(OnError) then OnError(errCode);
 end;
end;

function  glwGetLastError:cardinal;
begin
 result:=fLastError;
 fLastError:=0;
end;

function  glwTranslateError(errCode:cardinal):pchar;
var
 i:integer;
begin
 result:='Unknown error!';
 for i:=0 to high(fErrors) do
        if fErrors[i].code=errCode then begin
                result:=pchar(fErrors[i].translation);
                exit;
        end;
end;

procedure glwAddErrCode(errCode:cardinal; Translation:pchar);
var
 i:integer;
begin
 for i:=0 to high(fErrors) do
        if fErrors[i].code=errCode then begin
                fErrors[i].translation:=Translation;
                exit;
        end;
 setlength(fErrors, length(fErrors)+1);
 fErrors[high(fErrors)].code:=errCode;
 fErrors[high(fErrors)].translation:=translation;
end;

procedure glwInitErrors;
begin
 glwAddErrCode(0, 'No Error!');
 glwAddErrCode(1, 'Unable to destroy window! First destroy then rendering context!');
 glwAddErrCode(2, 'Unable to destroy window!');
 glwAddErrCode(3, 'Unable to un-register window class!');
 glwAddErrCode(4, 'Unable to use logging! File write error!');
 glwAddErrCode(5, 'Error writing to Log File!');
 glwAddErrCode(6, 'Failed to register the window class!');
 glwAddErrCode(7, 'Unable to create window!');
 glwAddErrCode(8, 'Performance timer not available!');
 glwAddErrCode(9, 'Window already created!');
 glwAddErrCode(10, 'Unavailable display mode!');
 glwAddErrCode(13, 'Window not created!');
 glwAddErrCode(14, 'GLW not initialized!');
 glwAddErrCode(16, 'GLW already initialized!');
 glwAddErrCode(18, 'Unable to De-initialize! First destroy the window!');
end;


procedure glwSaveFileFromStream(var S:TStream; filename:string); stdcall;
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 fs.SaveFileFromStream(s, filename);
end;

function  glwGetFile(filename:pchar; out size:cardinal):Pointer;
begin
 result:=nil;
 size:=0;
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 result:=fs.GetFile(filename, size);
end;

function  glwGetFileAsStream(filename:pchar; out size:cardinal):TMemoryStream;
begin
 result:=nil;
 size:=0;
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 result:=fs.GetFileAsStream(filename, size);
end;

function  glwGetLastErrorFs:pchar;
begin
 result:='';
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 result:=pchar(fs.LastError);
end;

procedure glwSetOnBeforeCreate(event:TOnBeforeCreate=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnBeforeCreate:=event;
end;

procedure glwSetOnAfterCreate(event:TOnAfterCreate=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 onAfterCreate:=event;
end;

procedure glwSetOnTick(event:TOnTick=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnTick:=event;
end;

procedure glwSetOnKeyUp(event:TOnKeyUp=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnKeyUp:=event;
end;

procedure glwSetOnKeyDown(event:TOnKeyDown=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnKeyDown:=event;
end;

procedure glwSetOnResize(event:TOnResize=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnResize:=event;
end;

procedure glwSetOnBeforeDestroy(event:TOnBeforeDestroy=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnBeforeDestroy:=event;
end;

procedure glwSetOnAfterDestroy(event:TOnAfterDestroy=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnAfterDestroy:=event;
end;

procedure glwSetOnClose(event:TOnClose=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnClose:=event;
end;

procedure glwSetOnActivate(event:TOnActivate=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnActivate:=event;
end;

procedure glwSetOnError(event:TOnError=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnError:=event;
end;

procedure glwSetOnMouseWheel(event:TOnMouseWheel=nil);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 OnMouseWheel:=event;
end;

procedure glwGetOnBeforeCreate(var event:TOnBeforeCreate);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnBeforeCreate;
end;

procedure glwGetOnAfterCreate(var event:TOnAfterCreate);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnAfterCreate;
end;

procedure glwGetOnTick(var event:TOnTick);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnTick;
end;

procedure glwGetOnKeyUp(var event:TOnKeyUp);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnKeyUp;
end;

procedure glwGetOnKeyDown(var event:TOnKeyDown);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnKeyDown;
end;

procedure glwGetOnResize(var event:TOnResize);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnResize;
end;

procedure glwGetOnBeforeDestroy(var event:TOnBeforeDestroy);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnBeforeDestroy;
end;

procedure glwGetOnAfterDestroy(var event:TOnAfterDestroy);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnAfterDestroy;
end;

procedure glwGetOnClose(var event:TOnClose);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnClose;
end;

procedure glwGetOnActivate(var event:TOnActivate);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnActivate;
end;

procedure glwGetOnError(var event:TOnError);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnError;
end;

procedure glwGetOnMouseWheel(var event:TOnMouseWheel);
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 event:=OnMouseWheel;
end;

function glwFPS:cardinal;
begin
 result:=fFPS;
end;

function glwDeltaTime:single;
begin
 result:=Delta;
end;

function glwExeName:pchar;
begin
 result:='';
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 result:=pchar(fExeName);
end;

function glwAppDir:pchar;
begin
 result:='';
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 result:=pchar(fAppDir);
end;

function glwIsActive:boolean;
begin
 result:=fActive;
end;

procedure UpdateKey(const Key:cardinal; const _on:boolean);
begin
 if (Key>255)then exit;
 keys[key]:=_on;
end;

function WndProc(hWnd: HWND; Msg: UINT;  wParam: WPARAM;  lParam: LPARAM): LRESULT; stdcall;
var
 i:integer;
 canClose:boolean;
begin
 for i:=0 to fEventListCount-1 do
        if msg=fEventList[i].event then begin
                if assigned(fEventList[i].proc) then begin
                        fEventList[i].proc(wParam,lParam);
                        result:=0;
                        exit;
                end;
        end;

 case (Msg) of
        WM_ACTIVATE: begin
                if LoWord(wParam)=WA_INACTIVE then fActive:=false
                                              else fActive:=true;
                if assigned(OnActivate) then OnActivate(fActive);
                result:=0;
        end;
        WM_CLOSE: begin
                canClose:=true;
                if assigned(OnClose) then OnClose(CanClose);
                if CanClose then begin
                        finished:=true;
                        Result := 0
                end
                else result:=1;
        end;

        WM_MOUSEWHEEL:begin
                if assigned(OnMouseWheel) then begin
                        if HiWord(wParam)<30000 then OnMouseWheel(HiWord(wParam)/WHEEL_DELTA, LoWord(lParam), HiWord(lParam), LoWord(wparam))
                                                else OnMouseWheel((HiWord(wParam)-65536)/WHEEL_DELTA, LoWord(lParam), HiWord(lParam), LoWord(wparam));
                end;
                Result:=0;
        end;

        WM_KEYDOWN: begin
                UpdateKey(wParam,true);
                Result := 0;
                if assigned(OnKeyDown) then OnKeyDown(wParam, lParam);
        end;

        WM_KEYUP: begin
                UpdateKey(wParam,false);
                Result := 0;
                if assigned(OnKeyUp) then OnKeyUp(wParam, lParam);
        end;

        WM_TIMER: begin
                if fFPS=0 then fFPS:=FPSCount;
                fFPS:=FPSCount;//(FPSCount+fFPS) div 2;
                FPSCount := 0;
                Result := 0;
        end;
        else
                Result :=DefWindowProc(hWnd, Msg, wParam, lParam);    // Default result if nothing happens
 end;
end;

function glwShiftState:boolean;
begin
 result:=Keys[VK_SHIFT];
end;

function glwIsKeyDown(const key:cardinal):boolean;
begin
 result:=keys[key];
end;

procedure clearLog;
var
 g:textfile;
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 if not fWriteLog then exit;
 fLogStartTime:=gettickcount;
 try
        deletefile(  fappdir+'Log.log');
        assignfile(g,fappdir+'Log.log');
        rewrite(g);
        closefile(g);
 except
        glwPostError(4);
        fWriteLog:=false;
 end;
end;

procedure glwAppendLog(s:string);
var
 f:textfile;
 s1:string;
 t:int64;
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 if not fWriteLog then exit;
 try
        t:=gettickcount;
        if length(s)>0 then begin
                s1:=inttostr((t-fLogStartTime)div 1000)+':'+inttostr((t-fLogStartTime) mod 1000);
                while length(s1)<10 do s1:=s1+' ';
                s:=pchar(s1+s);
        end;
        assignfile(f,fappdir+'Log.log');
        append(f);
        writeln(f,s);
        closefile(f);
 except
        glwPostError(5);
 end;
end;


procedure glwsetCmdLine(s:PChar);
var
 s1:string;
 i:integer;
begin
 {$WARNINGS OFF}
 CmdLine:=s;
 {$WARNINGS ON}
 fCmdLine:=ParamStr(0);
 s1:=fCmdLine;
 Delete(s1,1,pos('"',s1));
 Delete(s1,pos('"',s1),length(s1));
 fExeName:=s1;

 fappDir:='';
 repeat
        i:=Pos('\',s1);
        fappDir:=fappDir+copy(s1,1,i);
        delete(s1,1,i);
        i:=Pos('\',s1);
 until i=0;
end;

procedure DelEventHandler(event:cardinal);
var
 i,j:integer;
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 for i:=0 to fEventListCount-1 do
        if fEventList[i].event=event then begin
                dec(fEventListCount);
                for j:=i+1 to length(fEventList)-1 do
                        fEventList[j-1]:=fEventList[j];
                setLength(fEventList,length(fEventList)-1);
        end;
end;

procedure glwSetEventHandler(event:cardinal; proc:TEventProc);
var
 i:integer;
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 if not assigned(proc) then begin
        DelEventHandler(event);
        exit;
 end;
 for i:=0 to fEventListCount-1 do
        if fEventList[i].event=event then begin
                fEventList[i].proc:=proc;
                exit;
        end;
 setlength(fEventList,length(fEventList)+1);
 inc(fEventListCount);
 fEventList[fEventListCount-1].event:=event;
 fEventList[fEventListCount-1].proc:=proc;
end;

procedure glwGetEventHandler(event:cardinal; var proc:TEventProc);
var
 i:integer;
begin
 proc:=nil;
 if not fActive then begin
        glwPostError(14);
        exit;
 end;

 for i:=0 to fEventListCount-1 do
        if fEventList[i].event=event then begin
                proc:=fEventList[i].proc;
                exit;
        end;
 proc:=nil;
end;

procedure glwTick(var d:Single; var f:cardinal);
var
 cTime:int64;
begin
 Inc(FPSCount);
 QueryPerformanceCounter(cTime);
 delta:=((cTime-fLastTime)/freq);
 fLastTime:=cTime;
 d:=delta;
 f:=fFPS;
end;

procedure TimerProc(Wnd:HWND; uMsg:DWORD; idEvent:PDWORD; dwTime:DWORD); stdcall;
begin
 if fFPS=0 then fFPS:=FPSCount;
 fFPS:=FPSCount;
 FPSCount := 0;
end;

function glwWinHandle;
begin
 result:=h_Wnd;
end;

procedure glwCreateWindow;
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;

 {$WARNINGS OFF}
 NoErrMsg:=true;
 h_Wnd:=handle;

 SetTimer(h_Wnd, 1, 1000, @TimerProc);
 if not QueryPerformanceFrequency(fLastTime) then glwPostError(8);
 freq:=FlastTime/1000;
 QueryPerformanceCounter(fLastTime);
 glwAppendLog('Timer created.');
 {$WARNINGS ON}
end;

procedure glwInit;
var
 i:integer;
begin
 if fActive then begin
        glwPostError(16);
        exit;
 end;
 fWriteLog:=WriteLog;
 glwSetCmdLine(CmdLine);
 fActive:=true;
 fEventList:=nil;
 fEventListCount:=0;
 fLogStartTime:=0;
 clearLog;
 for i:=0 to 255 do keys[i]:=false;
 fs:=TFileSystem.create(fappDir+'Data\');
 finished:=true;
 h_Wnd:=0;
 delta:=0;
 fLastTime:=0;
 freq:=0;
 fFPS:=0;
 FPSCount:=0;
 fEventList:=nil;
 fEventListCount:=0;
 OnBeforeCreate:=nil;
 OnAfterCreate:=nil;
 OnTick:=nil;
 OnKeyUp:=nil;
 OnKeyDown:=nil;
 OnResize:=nil;
 OnBeforeDestroy:=nil;
 OnAfterDestroy:=nil;
 OnClose:=nil;
 OnActivate:=nil;
 OnError:=nil;
 OnMouseWheel:=nil;
 glwAppendLog('GLW started.');
end;

function glwVKtoASCII(const key: integer): integer;
var
 b:char;
 a:TKEyboardState;
begin
 b:=chr(1);
 GetKeyboardState(a);
 ToAscii(key,a[key],a,@b,0);
 if (ord(b)>=32)and(ord(b)<=(32+96)) then result:=ord(b)
                                     else result:=-1;
end;

procedure glwDeInit;
begin
 if not fActive then begin
        glwPostError(14);
        exit;
 end;
 fs.Free;
 glwAppendLog('GLW closed.');
 fActive:=false;
end;


initialization
 fLastError:=0;
 fErrors:=nil;
 glwInitErrors;

finalization
 finished:=true;
 glwDeInit;
end.
