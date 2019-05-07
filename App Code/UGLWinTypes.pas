unit UGLWinTypes;
interface
uses Windows, Classes;
type
 TEntry=record
        filename:shortstring;           {the name of the file}
        offset:cardinal;                {where the file's data begins}
        length:cardinal;                {the length in bytes of the file}
        nextEntryOffset:cardinal;       {the position of the next entry. is 0 if there are no more entries}
 end;

 TSettings=record
        fOnTop:boolean;
        fWidth, fHeight, fBPP, fRefreshRate:cardinal;
        fFullscreen:boolean;
 end;

 TEventProc = procedure(wParam: WPARAM;  lParam: LPARAM) of object;

 TOnBeforeCreate = procedure of object;
 TOnAfterCreate = procedure (handle:HDC)of object;
 TOnTick = procedure (const DeltaTime:single; const FPS:cardinal)of object;
 TOnKeyUp = procedure (const key:cardinal; const scanCode:cardinal)of object;
 TOnKeyDown = procedure (const key:cardinal; const scanCode:cardinal)of object;
 TOnResize = procedure (const W,H:cardinal)of object;
 TOnBeforeDestroy = procedure of object;
 TOnAfterDestroy = procedure of object;
 TOnClose = procedure(var canClose:boolean)of object;
 TOnActivate = procedure(const Active:boolean)of object;
 TOnError = procedure(errCode:cardinal)of object;
 TOnMouseWheel = procedure(const delta:single; const x,y:integer; const Keys:word)of object;

 TGlw=record
        IsActive:function:boolean; stdcall;
        IsWindowCreated:function:boolean; stdcall;
        Version:function:byte; stdcall;
        PostError:procedure(errCode:cardinal); stdcall;
        GetLastError:function:cardinal; stdcall;
        TranslateError:function(errCode:cardinal):pchar; stdcall;
        AddErrCode:procedure(errCode:cardinal; Translation:pchar); stdcall;

        Init:procedure(cmdLine:pchar; writeLog:boolean=true); stdcall;
        //need glwInit
                ExeName:function:pchar; stdcall;
                AppDir:function:pchar; stdcall;
                DeInit:procedure; stdcall;
                {Logging}
                AppendLog:procedure(s:string); stdcall;
                {events}
                SetOnBeforeCreate:procedure(event:TOnBeforeCreate=nil); stdcall;
                SetOnAfterCreate:procedure(event:TOnAfterCreate=nil); stdcall;
                SetOnTick:procedure(event:TOnTick=nil); stdcall;
                SetOnKeyUp:procedure(event:TOnKeyUp=nil); stdcall;
                SetOnKeyDown:procedure(event:TOnKeyDown=nil); stdcall;
                SetOnResize:procedure(event:TOnResize=nil); stdcall;
                SetOnBeforeDestroy:procedure(event:TOnBeforeDestroy=nil); stdcall;
                SetOnAfterDestroy:procedure(event:TOnAfterDestroy=nil); stdcall;
                SetOnClose:procedure(event:TOnClose=nil); stdcall;
                SetOnActivate:procedure(event:TOnActivate=nil); stdcall;
                SetOnError:procedure(event:TOnError=nil); stdcall;
                SetOnMouseWheel:procedure(event:TOnMouseWheel=nil); stdcall;
                SetEventHandler:procedure(event:cardinal; proc:TEventProc); stdcall;

                GetOnBeforeCreate:procedure(var event:TOnBeforeCreate); stdcall;
                GetOnAfterCreate:procedure(var event:TOnAfterCreate); stdcall;
                GetOnTick:procedure(var event:TOnTick); stdcall;
                GetOnKeyUp:procedure(var event:TOnKeyUp); stdcall;
                GetOnKeyDown:procedure(var event:TOnKeyDown); stdcall;
                GetOnResize:procedure(var event:TOnResize); stdcall;
                GetOnBeforeDestroy:procedure(var event:TOnBeforeDestroy); stdcall;
                GetOnAfterDestroy:procedure(var event:TOnAfterDestroy); stdcall;
                GetOnClose:procedure(var event:TOnClose); stdcall;
                GetOnActivate:procedure(var event:TOnActivate); stdcall;
                GetOnError:procedure(var event:TOnError); stdcall;
                GetOnMouseWheel:procedure(var event:TOnMouseWheel); stdcall;
                GetEventHandler:procedure(event:cardinal; var proc:TEventProc); stdcall;
                {Pak file}
                PakReplaceFile:procedure(filename, addAs:pchar); stdcall;
                PakReplaceFileFromStream:procedure(var S:TStream; addAs:pchar); stdcall;
                PakGetFile:function(filename:pchar; out size:cardinal):Pointer; stdcall;
                PakGetFileAsStream:function(filename:pchar; out size:cardinal):TMemoryStream; stdcall;
                PakGetLastError:function:pchar; stdcall;
                PakNumEntries:function:cardinal; stdcall;
                PakGetEntry:function(index:cardinal):TEntry; stdcall;
                PakAddFileFromStream:procedure (var S:TStream; addAs:string); stdcall;

                {Window Creation, info}
                CreateWindow:procedure(Caption:pchar); stdcall;
                        //need glwCreateWindow
                        WinHandle:function:HDC; stdcall;
                        IsOnTop:function:boolean; stdcall;
                        SetOnTop:procedure(value:boolean); stdcall;
                        GetDisplaySettings:function:TSettings; stdcall;
                        GetCaption:function:pchar; stdcall;
                        SetCaption:procedure(const value:pchar); stdcall;
                        FPS:function:cardinal; stdcall;
                        DeltaTime:function:single; stdcall;
                        IsKeyDown:function(const key:cardinal):boolean; stdcall;
                        VKtoASCII:function(const key:integer):integer; stdcall;
                        ShiftState:function:boolean; stdcall;
                        SetWinSize:procedure(Width, Height:cardinal); stdcall;
                        AvailableDisplayMode:function(fullscreen:boolean; Width, Height:cardinal; bpp:cardinal=32; RefreshRate:cardinal=75):boolean; stdcall;
                        SetFullScreen:procedure(fullscreen:boolean; Width, Height:cardinal; bpp:cardinal=32; RefreshRate:cardinal=75; forceIt:boolean=false); stdcall;
 end;

implementation
end.
