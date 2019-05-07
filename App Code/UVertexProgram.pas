{Done}
unit UVertexProgram;
interface
uses dglOpenGL, UTypes, UFormManager, Umaterials;

type
 TVPs=class
       private
        FVP:glUint;
        fAvailable:boolean;
        fOn:boolean;
        fVPs:array of TVertexProgram;
        errPos:cardinal;
        errString:string;
        error:string;
       public
        procedure SetCurrentVP(const VP : cardinal);
        function  AvailableVPext:boolean;
        procedure EnableVP;
        procedure DisableVP;
        procedure ClearVPs;
        function  AddVP(text:pchar):cardinal;
        procedure DelVP(vp:cardinal);
        procedure SetGlobalVPParameter(const index:cardinal; const x,y,z,w:single);
        procedure SetCurrentVPParameter(const index:cardinal; const x,y,z,w:single);

        constructor Create;
        destructor Destroy; override;

        procedure ApplyMaterial(mat:PMaterial); 
 end;
implementation
uses Urc, sysutils;

constructor TVPs.Create;
begin
 errPos:=0;
 errString:='';
 error:='';

 fAvailable:=GL_ARB_vertex_program;
 if not fAvailable then begin
        glwPostError(106);
        glwAppendLog('Vertex Program unavailable.');
        exit;
 end;
 FVP:=0;
 fVPs:=nil;
 GL.glDisable(GL_VERTEX_PROGRAM_ARB);
 fOn:=false;
 errPos:=0;
 errString:='';
 error:='';
 glwAppendLog('Vertex Program Manager created.');
end;

destructor TVPs.Destroy;
begin
 ClearVPs;
 GL.glDisable(GL_VERTEX_PROGRAM_ARB);
 inherited;
 glwAppendLog('Vertex Program Manager destroyed.');
end;

function  TVPs.AvailableVPext:boolean;
begin
 result:=fAvailable;
end;

procedure TVPs.SetCurrentVP(const VP: glUint);
begin
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 if vp=fvp then exit;
 GL.glBindProgramARB(GL_VERTEX_PROGRAM_ARB, vp);
 fVP:=vp;
end;

procedure TVPs.EnableVP;
begin
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 if fOn then exit;
 fOn:=true;
 GL.glEnable(GL_VERTEX_PROGRAM_ARB);
end;

procedure TVPs.DisableVP;
begin
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 if not fOn then exit;
 fOn:=false;
 GL.glDisable(GL_VERTEX_PROGRAM_ARB);
end;

procedure TVPs.ClearVPs;
var
 i:integer;
begin
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 for i:=0 to high(fVPs) do GL.glDeleteProgramsARB(1, @fVPs[i].fVP);
 fVPs:=nil;
 fVP:=0;
end;

function TVPs.AddVP(text:pchar):cardinal;
begin
 result:=0;
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 setlength(fVPs, length(fVPs)+1);
 GL.glGenProgramsARB( 1, @fVPs[high(fVPs)].fVP);
 result:=fVPs[high(fVPs)].fVP;

 GL.glBindProgramARB( GL_VERTEX_PROGRAM_ARB, result);
 fVP:=result;

 GL.glProgramStringARB(GL_VERTEX_PROGRAM_ARB, GL_PROGRAM_FORMAT_ASCII_ARB,length(text),text);

 if GL.glGetError=GL_INVALID_OPERATION then begin
        GL.glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB,@errPos);
        errString:=GL.glGetString(GL_PROGRAM_ERROR_STRING_ARB);
        error:='Renderer.dll: Error '''+errString+''' at position '+inttostr(errPos);
        glwAddErrCode(107, pchar(error));
        glwPostError(107);
        error:='';
        errString:='';
        errPos:=0;
        GL.glDeleteProgramsARB(1, @fVPs[high(fVPs)].fVP);
        setlength(fVPs, high(fVPs));
        fVP:=0;
        result:=0;
 end;
end;

procedure TVPs.DelVP(vp:cardinal);
var
 i,j:integer;
begin
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 for i:=0 to high(fVPs) do
        if fVPs[i].fVP=vp then begin
                GL.glDeleteProgramsARB(1, @fVPs[i].fVP);
                for j:=i+1 to high(fVPs) do fVPs[j-1]:=fVPs[j];
                setlength(fVPs, high(fVPs));
        end;
end;

procedure TVPs.SetGlobalVPParameter(const index:cardinal; const x,y,z,w:single);
begin
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 GL.glProgramEnvParameter4fARB(GL_VERTEX_PROGRAM_ARB, index, x, y, z, w );
end;

procedure TVPs.SetCurrentVPParameter(const index:cardinal; const x,y,z,w:single);
begin
 if not fAvailable then begin
        glwPostError(106);
        exit;
 end;
 GL.glProgramLocalParameter4fARB(GL_VERTEX_PROGRAM_ARB, index, x, y, z, w );
end;

procedure TVPs.ApplyMaterial(mat: PMaterial);
begin
 if mat.VPSettings.UseVP then begin
        if not fOn then begin
                fOn:=true;
                GL.glEnable(GL_VERTEX_PROGRAM_ARB);
        end;
        if mat.VPSettings.VP=fvp then exit;
        GL.glBindProgramARB(GL_VERTEX_PROGRAM_ARB, mat.VPSettings.VP);
        fVP:=mat.VPSettings.VP;
 end
 else begin
        if fOn then begin
                fOn:=false;
                GL.glDisable(GL_VERTEX_PROGRAM_ARB);
        end;
 end;
end;

end.
