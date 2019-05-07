{In progress}
unit USkyBox;
interface
uses UFormManager, UTypes, URC, UCamera, UMaterials, xSL;
type
 TSkyBox=class
        private
         fOk:boolean;
         fMaterial:PMaterial;
         fFovY:single;
         fNumTex:byte;

         fDL:cardinal;

         CPOF1, cpof2:single;
         CPOffset1:single;
         CPOffset2:single;
         CPSize1:single;
         CPSize2:single;
         CPSpeed1:single;
         CPSpeed2:single;
         CP1Alpha:single;
         CP2Alpha:single;
         procedure SetCloudPlane1Offset(offset:single);
         procedure SetCloudPlane2Offset(offset:single);
         function GetCloudPlane1Offset:single;
         function GetCloudPlane2Offset:single;

         procedure SetCloudPlane1Size(Size:single);
         procedure SetCloudPlane2Size(Size:single);
         function GetCloudPlane1Size:single;
         function GetCloudPlane2Size:single;

         procedure SetCloudPlane1Speed(Speed:single);
         procedure SetCloudPlane2Speed(Speed:single);
         function GetCloudPlane1Speed:single;
         function GetCloudPlane2Speed:single;

         function _Load(Args: TArray; var error: pchar): Variant;
         function _Clear(Args: TArray; var error: pchar): Variant;
        public
         Constructor Create;

         procedure AddScriptFunctions(Script:PScript);
         procedure Load(Name:string);   //numele skybox-ului
                                        //include fisierele SkyBox\nume.bmp si SkyBox\nume.skb
         procedure Clear;
         destructor destroy; override;
         procedure render;

         property CloudPlane1Offset:single read getCloudPlane1Offset write setCloudPlane1Offset;
         property CloudPlane2Offset:single read getCloudPlane2Offset write setCloudPlane2Offset;

         property CloudPlane1Size:single read getCloudPlane1Size write setCloudPlane1Size;
         property CloudPlane2Size:single read getCloudPlane2Size write setCloudPlane2Size;

         property CloudPlane1Speed:single read getCloudPlane1Speed write setCloudPlane1Speed;
         property CloudPlane2Speed:single read getCloudPlane2Speed write setCloudPlane2Speed;

         property CloudPlane1Alpha:single read CP1Alpha write CP1Alpha;
         property CloudPlane2Alpha:single read CP2Alpha write CP2Alpha;
         property OK:boolean read fOk;
 end;
implementation
uses Classes, sysutils, dglOpenGL, URenderer;
{ TSkyBox }
procedure TSkyBox.AddScriptFunctions;
begin
 Script.AddFunction('skyboxload',1,_Load);
 Script.AddFunction('skyboxclear',0,_Clear);
end;

procedure TSkyBox.Clear;
begin
 if fMaterial.TexStg.texture0<>nil then Renderer.RC.fTexManager.DeleteTexture(fmaterial.texStg.texture0);
 if fDL<>0 then GL.glDeleteLists(fDL, 1);
 fMaterial.TexStg.texture0:=nil;
 fDL:=0;
 fOk:=false;
end;

constructor TSkyBox.Create;
begin
 inherited create;
 fFovY:=0;
 fNumTex:=0;
 fDL:=0;
 CPOF1:=0;
 cpof2:=0;
 CPOffset1:=0.2;
 CPOffset2:=0.201;
 CPSize1:=5;
 CPSize2:=10;
 CPSpeed1:=1/200000;
 CPSpeed2:=1/30000;
 CP1Alpha:=0.3;
 CP2Alpha:=0.3;
 fMaterial:=NewMaterial;
 fMaterial.TexStg.texture0:=nil;
 fMaterial.TexStg.texture1:=nil;
 fOk:=false;

 fMaterial.TexStg.UseTex0:=true;
 fMaterial.TexStg.UseTex1:=false;
 fMaterial.UseCulling:=true;
 fMaterial.DepthTest.DepthTest:=false;
 fMaterial.UseFog:=false;
 fMaterial.DynamicalyIluminated:=false;
 fMaterial.Blend.Use:=false;
 fMaterial.Blend.fSrc:=GL_SRC_ALPHA;
 fMaterial.Blend.fDst:=GL_ONE_MINUS_SRC_ALPHA;
 fMaterial.UseScissorTest:=false;
 fMaterial.UseStencilTest:=false;
 fMaterial.UseRegisterCombiners:=false;
 fMaterial.VertexArray.VertexArray:=false;
 fMaterial.VertexArray.ColorArray:=false;
 fMaterial.VertexArray.TexCoord0Array:=false;
 fMaterial.VertexArray.TexCoord1Array:=false;
 fMaterial.VertexArray.NormalArray:=false;
 fMaterial.DepthTest.DepthMask:=false;
 fMaterial.Color[0]:=1;
 fMaterial.Color[1]:=1;
 fMaterial.Color[2]:=1;
 fMaterial.Color[3]:=1;
 glwAppendLog('Skybox created.');
end;

destructor TSkyBox.destroy;
begin
 if fDL<>0 then GL.glDeleteLists(fDL, 1);
 ClearMaterial(fMaterial);
 glwAppendLog('Skybox destroyed.');
 inherited;
end;

function TSkyBox.GetCloudPlane1Offset: single;
begin
 result:=CPOffset1;
end;

function TSkyBox.GetCloudPlane1Size: single;
begin
 result:=CPSize1;
end;

function TSkyBox.GetCloudPlane1Speed: single;
begin
 result:=CPSpeed1;
end;

function TSkyBox.GetCloudPlane2Offset: single;
begin
 result:=CPOffset2;
end;

function TSkyBox.GetCloudPlane2Size: single;
begin
 result:=CPSize2;
end;

function TSkyBox.GetCloudPlane2Speed: single;
begin
 result:=CPSpeed2;
end;

procedure TSkyBox.Load(Name: string);
var
 s:TMemoryStream;
 sl:TStrings;
 size:cardinal;
 t:string;
begin
 Clear;
 
 t:='SkyBox\'+Name+'.bmp';
 fMaterial.TexStg.texture0:=Renderer.rc.fTexManager.LoadTexture(pchar(t));
 if fMaterial.TexStg.texture0=nil then begin
        t:='SkyBox\'+Name+'.tga';
        fMaterial.TexStg.texture0:=Renderer.rc.fTexManager.LoadTexture(pchar(t));
 end;
 if fMaterial.TexStg.texture0=nil then begin
        t:='SkyBox\'+Name+'.jpg';
        fMaterial.TexStg.texture0:=Renderer.rc.fTexManager.LoadTexture(pchar(t));
 end;
 if fMaterial.TexStg.texture0=nil then exit;

 s:=glwGetFileAsStream(pchar(changeFileext(t, '.skb')), size);
 t:=glwGetLastErrorFs;
 if t<>'' then begin
        Renderer.RC.fTexManager.DeleteTexture(fMaterial.TexStg.texture0);
        fMaterial.TexStg.texture0:=nil;
        exit;
 end;
 sl:=TStringList.Create;
 sl.LoadFromStream(s);
 fNumTex:=strtoint(sl.Strings[0]);
 fFovY:=strtoint(sl.Strings[1]);

 if fNumTex>6 then begin
        CPOffset1:=strtofloat(sl.Strings[3]);
        CPSize1  :=strtofloat(sl.Strings[4]);
        CPSpeed1 :=strtofloat(sl.Strings[5]);
        CP1Alpha :=strtofloat(sl.Strings[6]);
 end;

 if fNumTex=8 then begin
        CPOffset2:=strtofloat(sl.Strings[8]);
        CPSize2  :=strtofloat(sl.Strings[9]);
        CPSpeed2 :=strtofloat(sl.Strings[10]);
        CP2Alpha :=strtofloat(sl.Strings[11]);
 end;
 sl.Free;
 s.Free;

 fDL:=GL.glGenLists(1);
 GL.glNewList(fDL, GL_COMPILE);
        //Back
        GL.glBegin(GL_QUADS);
                GL.glTexCoord2f(0.00025, 0.998);  GL.glVertex3f(1,1,1);
                GL.glTexCoord2f(0.00025, 0.002);  GL.glVertex3f(1,-1,1);
                GL.glTexCoord2f(0.125, 0.002);  GL.glVertex3f(-1,-1,1);
                GL.glTexCoord2f(0.125, 0.998);  GL.glVertex3f(-1,1,1);
        GL.glEnd;

        //Left
        GL.glBegin(GL_QUADS);
                GL.glTexCoord2f(0.125, 0.998);  GL.glVertex3f(-1,1,1);
                GL.glTexCoord2f(0.125, 0.002);  GL.glVertex3f(-1,-1,1);
                GL.glTexCoord2f(0.25, 0.002);  GL.glVertex3f(-1,-1,-1);
                GL.glTexCoord2f(0.25, 0.998);  GL.glVertex3f(-1,1,-1);
        GL.glEnd;

        //Front
        GL.glBegin(GL_QUADS);
                GL.glTexCoord2f(0.25, 0.998);  GL.glVertex3f(-1,1,-1);
                GL.glTexCoord2f(0.25, 0.002);  GL.glVertex3f(-1,-1,-1);
                GL.glTexCoord2f(0.375, 0.002);  GL.glVertex3f(1,-1,-1);
                GL.glTexCoord2f(0.375, 0.998);  GL.glVertex3f(1,1,-1);
        GL.glEnd;

        //Right
        GL.glBegin(GL_QUADS);
                GL.glTexCoord2f(0.375, 0.998);  GL.glVertex3f(1,1,-1);
                GL.glTexCoord2f(0.375, 0.002);  GL.glVertex3f(1,-1,-1);
                GL.glTexCoord2f(0.5-0.00025, 0.002);  GL.glVertex3f(1,-1,1);
                GL.glTexCoord2f(0.5-0.00025, 0.998);  GL.glVertex3f(1,1,1);
        GL.glEnd;

        //Up
        GL.glBegin(GL_QUADS);
                GL.glTexCoord2f(0.5+0.00025, 0.998);  GL.glVertex3f(-1,1,1);
                GL.glTexCoord2f(0.5+0.00025, 0.002);  GL.glVertex3f(-1,1,-1);
                GL.glTexCoord2f(0.625-0.00025, 0.002);  GL.glVertex3f(1,1,-1);
                GL.glTexCoord2f(0.625-0.00025, 0.998);  GL.glVertex3f(1,1,1);
        GL.glEnd;

        //Down
        GL.glBegin(GL_QUADS);
                GL.glTexCoord2f(0.625+0.00025, 0.998);  GL.glVertex3f(-1,-1,-1);
                GL.glTexCoord2f(0.625+0.00025, 0.002);  GL.glVertex3f(-1,-1,1);
                GL.glTexCoord2f(0.75-0.00025, 0.002);  GL.glVertex3f(1,-1,1);
                GL.glTexCoord2f(0.75-0.00025, 0.998);  GL.glVertex3f(1,-1,-1);
        GL.glEnd;
 GL.glEndList;
 fOk:=true;
 glwAppendLog('Skybox "'+name+'"loaded.');
end;

procedure TSkyBox.render;
var
 x,y,z:glFloat;
 f:single;
begin
 if not fOk then exit;
 if not Renderer.rendering then exit;
 fMaterial.Blend.Use:=false;
 Renderer.RC.fStateManager.ApplyMaterial(fMaterial);
 Renderer.RC.fTexManager.ApplyMaterial(fMaterial);

 GL.glColor3f(1,1,1);
 x:=Renderer.Camera.Position[0];
 y:=Renderer.Camera.Position[1];
 z:=Renderer.Camera.Position[2];

 f:=Renderer.Camera.fovY;
 Renderer.Camera.fovY:=fFovY;
 GL.glTranslatef(x,y,z);


 GL.glCallList(fDL);

 GL.glTranslatef(0,-Renderer.Camera.zNear,0);
 fMaterial.Blend.Use:=true;
 Renderer.RC.fStateManager.ApplyMaterial(fMaterial);

 if CPOffSet2>CPOffSet1 then begin
        if fNumTex=8 then begin
                GL.glColor4F(1,1,1,CP2Alpha);
                GL.glBegin(GL_QUADS);
                        GL.glTexCoord2f(0.875-0.00025+0.0625, CPOF2);    GL.glVertex3f(-CPSize2,+CPOffset2,+CPSize2);
                        GL.glTexCoord2f(0.875-0.00025+0.0625, CPOF2-1);  GL.glVertex3f(-CPSize2,+CPOffset2,-CPSize2);
                        GL.glTexCoord2f(0.875+0.00025, CPOF2-1);    GL.glVertex3f(+CPSize2,+CPOffset2,-CPSize2);
                        GL.glTexCoord2f(0.875+0.00025, CPOF2);      GL.glVertex3f(+CPSize2,+CPOffset2,+CPSize2);
                GL.glEnd;
                cpof2:=cpof2+Renderer.DeltaTime*CPSpeed2;
        end;


        if fNumTex>6 then begin
                GL.glColor4F(1,1,1,CP1Alpha);
                GL.glBegin(GL_QUADS);
                        GL.glTexCoord2f(0.75-0.00025+0.0625, CPOF1);    GL.glVertex3f(-CPSize1,+CPOffset1,+CPSize1);
                        GL.glTexCoord2f(0.75-0.00025+0.0625, CPOF1-1);  GL.glVertex3f(-CPSize1,+CPOffset1,-CPSize1);
                        GL.glTexCoord2f(0.75+0.00025, CPOF1-1);    GL.glVertex3f(+CPSize1,+CPOffset1,-CPSize1);
                        GL.glTexCoord2f(0.75+0.00025, CPOF1);      GL.glVertex3f(+CPSize1,+CPOffset1,+CPSize1);
                GL.glEnd;
                cpof1:=cpof1+Renderer.DeltaTime*CPSpeed1;
        end;
 end
 else begin
        if fNumTex>6 then begin
                GL.glColor4F(1,1,1,CP1Alpha);
                GL.glBegin(GL_QUADS);
                        GL.glTexCoord2f(0.75-0.00025+0.0625, CPOF1);    GL.glVertex3f(-CPSize1,+CPOffset1,+CPSize1);
                        GL.glTexCoord2f(0.75-0.00025+0.0625, CPOF1-1);  GL.glVertex3f(-CPSize1,+CPOffset1,-CPSize1);
                        GL.glTexCoord2f(0.75+0.00025, CPOF1-1);    GL.glVertex3f(+CPSize1,+CPOffset1,-CPSize1);
                        GL.glTexCoord2f(0.75+0.00025, CPOF1);      GL.glVertex3f(+CPSize1,+CPOffset1,+CPSize1);
                GL.glEnd;
                cpof1:=cpof1+Renderer.DeltaTime*CPSpeed1;
        end;

        if fNumTex=8 then begin
                GL.glColor4F(1,1,1,CP2Alpha);
                GL.glBegin(GL_QUADS);
                        GL.glTexCoord2f(0.875-0.00025+0.0625, CPOF2);    GL.glVertex3f(-CPSize2,+CPOffset2,+CPSize2);
                        GL.glTexCoord2f(0.875-0.00025+0.0625, CPOF2-1);  GL.glVertex3f(-CPSize2,+CPOffset2,-CPSize2);
                        GL.glTexCoord2f(0.875+0.00025, CPOF2-1);    GL.glVertex3f(+CPSize2,+CPOffset2,-CPSize2);
                        GL.glTexCoord2f(0.875+0.00025, CPOF2);      GL.glVertex3f(+CPSize2,+CPOffset2,+CPSize2);
                GL.glEnd;
                cpof2:=cpof2+Renderer.DeltaTime*CPSpeed2;
        end;
 end;

 GL.glTranslatef(-x,-y,-z);
 Renderer.Camera.fovY:=f;
end;

procedure TSkyBox.SetCloudPlane1Offset(offset: single);
begin
 CPOffset1:=offset;
end;

procedure TSkyBox.SetCloudPlane1Size(Size: single);
begin
 CPSIze1:=size;
end;

procedure TSkyBox.SetCloudPlane1Speed(Speed: single);
begin
 CPSpeed1:=Speed;
end;

procedure TSkyBox.SetCloudPlane2Offset(offset: single);
begin
 CPOffset2:=offset;
end;

procedure TSkyBox.SetCloudPlane2Size(Size: single);
begin
 CPSize2:=Size;
end;

procedure TSkyBox.SetCloudPlane2Speed(Speed: single);
begin
 CPSpeed2:=Speed;
end;

function TSkyBox._Clear(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 Clear;
end;

function TSkyBox._Load(Args: TArray; var error: pchar): Variant;
begin
 error:='';
 Load(Args[0]);
end;

end.
