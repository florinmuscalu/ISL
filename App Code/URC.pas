{In progress... add xSLFunction}
unit Urc;
interface
uses windows, dglOpenGL, UFormManager, UTypes, UStateManager, UTexManager, UVertexProgram, xSL;

type
 TOnAfterGLInit = procedure of object;
 TOnBeforeGLDestroy = procedure of object;

 TRC=class
        private
         fGLActive:boolean;
         fGLSaved8087CW: Word;
         fDC:HDC;
         fRC:HGLRC;
         fOnAfterGLInit:TOnAfterGLInit;
         fOnBeforeGLDestroy:TOnBeforeGLDestroy;
        public
         fStateManager:TStateManager;
         fTexManager:TTexManager;
         fVPs:TVPs;

         //Adauga functiile pentru xSL
         procedure AddScriptFunctions(ScriptEngine:PScript);

         
         property IsGLActive:boolean read fGLActive;
         property OnAfterGLInit:TOnAfterGLInit read fOnAfterGlInit write fOnAfterGLInit;
         property OnBeforeGLDestroy:TOnBeforeGLDestroy read fOnBeforeGLDestroy write fOnBeforeGLDestroy;
         property DC:HDC read fDC;

         constructor Create(OnAfterGLInit:TOnAfterGLInit; ColorBits, DepthBits, StencilBits:byte);
         destructor Destroy; override;
         procedure GLFlip(const complete:boolean);
         procedure VSync(_on: boolean; forceIt:boolean=false);
 end;
implementation
uses Umath;//, UVBOs;

function ChangeDisplaySettings(lpDevMode: PDeviceModeA; dwFlags: DWORD): Longint; stdcall;  external user32 name 'ChangeDisplaySettingsA';
{ TRC }

procedure TRC.AddScriptFunctions(ScriptEngine: PScript);
begin

end;

constructor TRC.Create(OnAfterGLInit:TOnAfterGLInit; ColorBits, DepthBits, StencilBits: byte);
begin
 if fGLActive then begin
        GLWPostError(15);
        exit;
 end;

 fGLActive:=false;
 fDC:=0;
 fRC:=0;
 fOnAfterGLInit:=OnAfterGLInit;
 OnBeforeGLDestroy:=nil;


 glwAddErrCode(11, 'OpenGL Context not initialized!');
 glwAddErrCode(12, 'Error initializing openGL!');
 glwAddErrCode(15, 'OpenGL Context already initialized!');
 glwAddErrCode(100, 'Texture not found!');
 glwAddErrCode(101, 'Invalid texture!');
 glwAddErrCode(102, 'Index out of bounds!');
 glwAddErrCode(103, 'Invalid texture name!');
 glwAddErrCode(104, 'First bind the GLWin Routines using ''LoadGLWin''!');
 glwAddErrCode(106, 'Extension GL_ARB_Vertex_program unavailable!');
 glwAddErrCode(109, 'Invalid vertex program!');
 glwAddErrCode(108, 'Extension GL_ARB_Vertex_Buffer_Object unavailable!');

 fGLSaved8087CW := Default8087CW;
 Set8087CW($133f); { Disable all fpu exceptions }
 ShowCursor(false);

 if not InitOpenGL then begin
        GLWPostError(12);
        GLWAppendLog('Error initializing openGL!');
        MessageBox(glwWinHandle, 'Error initializing openGL!', 'Fatal error',MB_OK or MB_ICONERROR);
        halt;
 end;
 fDC:=getDC(glwWinHandle);
 fRC:=CreateRenderingContext(fDC,[opDoubleBuffered],ColorBits, DepthBits, StencilBits,0,0,0);
 ActivateRenderingContext(fDC,fRC);
 fGLActive:=true;
 VSync(true, true);

 fStateManager:=TStateManager.Create(self);
 fTexManager:=TTexManager.create;
 fVPs:=TVPs.Create;
// VBOCreate;
 if assigned(OnAfterGLInit) then OnAfterGLInit;
 glwAppendLog('GLContext initialized.');
end;

destructor TRC.Destroy;
begin
 if not fGLActive then begin
        glwPostError(11);
        exit;
 end;
 try
        if assigned(OnBeforeGLDestroy) then OnBeforeGLDestroy;
//        VBODestroy;
        fVPs.Free;
        fTexManager.Free;
        fStateManager.Free;
        Set8087CW(fGLSaved8087CW);
        ShowCursor(true);
        //ChangeDisplaySettings(nil, CDS_FULLSCREEN);
        if fRC <> 0 then begin
                wglMakeCurrent(0, 0);
                wglDeleteContext(fRC);
        end;
        ReleaseDC(glwWinHandle, fDC);
 except end;
 glwAppendLog('GLContext destroyed.');
 fGLActive:=false;
 inherited;
end;

procedure TRC.GLFlip(const complete: boolean);
begin
 if not fGLActive then begin
        glwPostError(11);
        exit;
 end;
 if complete then GL.glFinish
             else GL.glFlush;
 swapbuffers(fDC);
end;

procedure TRC.VSync(_on, forceIt: boolean);
var
 i:integer;
begin
 if not fGLActive then begin
       glwPostError(11);
       exit;
 end;
 if WGL_EXT_swap_control then begin
        i := GL.wglGetSwapIntervalEXT;
        if _on then begin
                if i<>1 then GL.wglSwapIntervalEXT(1);
                glwAppendLog('VSync on.');
        end
        else begin
                if i<>0 then GL.wglSwapIntervalEXT(0);
                glwAppendLog('VSync off.');
        end;
 end
 else glwAppendLog('WGL_EXT_swap_control extension unavailable!');
end;

end.
